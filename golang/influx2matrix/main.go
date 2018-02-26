package main

import (
	"encoding/csv"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"math"
	"os"
	"reflect"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"time"

	client "github.com/influxdata/influxdb/client/v2"
)

const (
	db             = "telegraf"
	iHost          = "https://ADDR:8086"
	iUser          = "USER"
	iPass          = "PASS"
	iSslSkipVerify = true
	iUA            = "trippy"
	outDir         = "/srv/data/"
	//outDir         = "/home/markus/projects/stream-data-mining/data/tut-dm-final/"
	//dockerDir      = "/mnt/tut-dm-final/"
	timeLayout = "2006-01-02 15:04:05"
)

// queryDB convenience function to query the database
func queryDB(clnt client.Client, cmd string) (res []client.Result, err error) {
	q := client.Query{
		Command:  cmd,
		Database: db,
		Chunked:  true,
	}
	if response, err := clnt.Query(q); err == nil {
		if response.Error() != nil {
			return res, response.Error()
		}
		res = response.Results
	} else {
		return res, err
	}
	return res, nil
}

func deleteEmpty(s map[string]string) (r []string) {
	for _, str := range s {
		if str != "" {
			r = append(r, str)
		}
	}
	return
}

func map2sortedSlice(s map[string]string) (r []string) {
	var keys []string
	for k, v := range s {
		if v != "" {
			keys = append(keys, k)
		}
	}
	sort.Strings(keys)
	for _, k := range keys {
		r = append(r, s[k])
	}
	return
}

func interface2string(items []interface{}) (strings []string) {
	for _, item := range items {
		switch v := item.(type) {
		case json.Number:
			strings = append(strings, string(v))
		case string:
			strings = append(strings, v)
		case nil:
			strings = append(strings, "0")
		default:
			log.Fatal(reflect.TypeOf(item))
		}
	}
	return
}

func float2string(items []float64) (strings []string) {
	for _, item := range items {
		strings = append(strings, strconv.FormatFloat(item, 'f', 8, 64))
	}
	return
}

func remove(slice []string, s int) []string {
	return append(slice[:s], slice[s+1:]...)
}
func removeForInterface(slice []interface{}, s int) []interface{} {
	return append(slice[:s], slice[s+1:]...)
}

func createLabels(columns, tags []string, measurement string) []string {
	t := strings.Join(tags, "_")
	t = strings.Join([]string{measurement, t}, "_")
	for i, v := range columns {
		arr := []string{t, v}
		columns[i] = strings.Join(arr, "_")
	}
	return columns
}

func showMeasurements() [][]interface{} {
	c, err := client.NewHTTPClient(client.HTTPConfig{
		Addr:               iHost,
		UserAgent:          iUA,
		Username:           iUser,
		Password:           iPass,
		InsecureSkipVerify: iSslSkipVerify,
	})
	if err != nil {
		log.Fatal(err)
	}
	if c == nil {
		log.Fatal(err)
	}
	resp, err := queryDB(c, "SHOW MEASUREMENTS")
	if err != nil {
		log.Fatal(err)
	}
	c.Close()
	return resp[0].Series[0].Values
}

func midnight(days uint) (from, to string, delta uint) {
	t, _ := time.LoadLocation("UTC")
	now := time.Now().In(t)
	then := now.Add(-24 * time.Duration(days) * time.Hour)
	year, month, day := now.Date()
	year2, month2, day2 := then.Date()

	toTime := time.Date(year, month, day, 0, 0, 0, 0, now.Location())
	fromTime := time.Date(year2, month2, day2, 0, 0, 0, 0, now.Location())

	to = toTime.Format(timeLayout)
	from = fromTime.Format(timeLayout)

	delta = uint(toTime.Sub(fromTime).Minutes()) * 60
	return
}

func today() (from, to string, delta uint) {
	t, _ := time.LoadLocation("UTC")
	now := time.Now().In(t)

	year, month, day := now.Date()
	hr, min, _ := now.Clock()

	fromTime := time.Date(year, month, day, 0, 0, 0, 0, now.Location())
	toTime := time.Date(year, month, day, hr, min, 0, 0, now.Location())

	from = fromTime.Format(timeLayout)
	to = toTime.Format(timeLayout)
	delta = uint(toTime.Sub(fromTime).Minutes()) * 60
	return
}

func custom(days uint, to string) (from string, delta uint) {
	t, err := time.Parse(timeLayout, to)
	if err != nil {
		fmt.Println(err)
	}
	then := t.Add(-24 * time.Duration(days) * time.Hour)
	year, month, day := then.Date()
	hr, min, sec := then.Clock()

	fromTime := time.Date(year, month, day, hr, min, sec, 0, t.Location())

	from = fromTime.Format(timeLayout)
	delta = uint(t.Sub(then).Minutes()) * 60
	return
}

// Data struct holds data
type Data struct {
	metrix [][]float64
	labels []string

	bulk    uint
	split   bool
	binSize uint
	cycles  uint
	anon    bool
}

// InitData inits data. Pretty self-expalitory
func InitData(split bool, days, delta, binSize uint, anon bool) (d *Data, err error) {
	bins := uint(math.Ceil(float64(delta / binSize)))
	d = &Data{
		metrix:  make([][]float64, bins),
		bulk:    bins / days,
		split:   split,
		binSize: binSize,
		cycles:  days,
		anon:    anon,
	}
	return d, nil
}

func (d *Data) unmarshalMetrix(slice []interface{}, t int) *Data {
	s2 := make([]float64, len(slice)-1)
	for i, val := range slice[1:] {
		switch val.(type) {
		case json.Number:
			s2[i], _ = val.(json.Number).Float64()
		case nil:
			s2[i] = 0
		}
	}
	d.metrix[t] = append(d.metrix[t], s2...)
	return d
}

func (d *Data) getDataFromInflux(measurement, from, to string) *Data {
	c, err := client.NewHTTPClient(client.HTTPConfig{
		Addr:               iHost,
		UserAgent:          iUA,
		Username:           iUser,
		Password:           iPass,
		InsecureSkipVerify: iSslSkipVerify,
	})
	if err != nil {
		log.Fatal(err)
	}
	if c == nil {
		log.Fatal(err)
	}
	q := []string{
		"SELECT mean(*) FROM ",
		measurement,
		" WHERE time > '",
		from,
		"' AND time < '",
		to,
		"' GROUP BY host, node_name, path, interface, time(",
		strconv.Itoa(int(d.binSize)),
		"s)",
	}
	log.Println(strings.Join(q, ""))
	resp, err := queryDB(c, strings.Join(q, ""))
	if err != nil {
		log.Fatal(err)
	}
	var partial bool
	if resp[0].Series[0].Partial == true {
		partial = true
	}
	for i, item := range resp {
		data := item.Series[0]
		labels := createLabels(remove(data.Columns, 0), map2sortedSlice(data.Tags), measurement)
		metrix := data.Values
		if partial == false {
			d.labels = append(d.labels, labels...)
			for time, series := range metrix {
				d.unmarshalMetrix(series, time)
			}
		} else if data.Partial == true {
			for j := i + 1; i < len(resp); j++ {
				metrix = append(metrix, resp[j].Series[0].Values...)
				if resp[j].Series[0].Partial == false {
					d.labels = append(d.labels, labels...)
					for time, series := range metrix {
						d.unmarshalMetrix(series, time)
					}
					break
				}
			}
		}
	}
	runtime.GC()
	c.Close()
	log.Println("Collected query, total in memory: ", len(d.labels), "columns and", len(d.metrix), "rows")
	return d
}

func (d *Data) flush2csv() *Data {
	log.Println("writing csv")
	j := 0
	k := 1
	labels := make([]string, len(d.labels))

	if d.anon == true {
		for i := range d.labels {
			labels[i] = strconv.Itoa(i)
		}
	} else {
		labels = d.labels
	}
	if d.split == true {
		for i := 0; i <= len(d.metrix); i++ {
			if (i%(int(d.bulk)) == 0 || i%len(d.metrix) == 0) && i > 0 {
				outfile := []string{
					outDir,
					strconv.Itoa(k),
					".csv",
				}
				file, err := os.Create(strings.Join(outfile, ""))
				if err != nil {
					log.Fatal(err)
				}
				log.Println("Writing", outfile, i)
				writer := csv.NewWriter(file)
				writer.Write(labels)
				for _, value := range d.metrix[j:i] {
					writer.Write(float2string(value))
				}
				writer.Flush()
				file.Close()
				j = i
				k++
			}
		}
	} else {
		outfile := []string{
			outDir,
			"data",
			".csv",
		}
		for i := 0; i <= len(d.metrix); i++ {
			if i%len(d.metrix) == 0 && i > 0 {
				file, err := os.Create(strings.Join(outfile, ""))
				if err != nil {
					log.Fatal(err)
				}
				log.Println("Writing", outfile, i)
				writer := csv.NewWriter(file)
				writer.Write(labels)
				for _, value := range d.metrix[j:i] {
					writer.Write(float2string(value))
				}
				writer.Flush()
				file.Close()
				j = i
				k++
			}
		}
	}
	return d
}

func (d *Data) writeCSV(outfile []string) *Data {
	file, err := os.Create(strings.Join(outfile, ""))
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	writer := csv.NewWriter(file)
	defer writer.Flush()

	writer.Write(d.labels)
	for _, value := range d.metrix {
		writer.Write(float2string(value))
	}
	return d
}

func main() {
	var (
		usecase = flag.String("usecase", "today", "today, yesterday or custom string in 'YYYY-MM-DD hh-mm-ss' format")
		split   = flag.Bool("split", false, "split output files per day or just write single massive csv")
		days    = flag.Uint("days", 3, "number of days to pull. Not used if maximum is 'today'")
		bSize   = flag.Uint("bsize", 60, "size of single bucket or bin, i.e. value of 60 would mean that data points will be aggregated to a period of 1 minute")
		anon    = flag.Bool("anon", false, "--anon to remove label names and mine some sweet data on unencryted home PC-s")
	)
	flag.Parse()

	var from string
	var to string
	var cycles uint
	var delta uint

	measurements := []string{
		"cpu",
		"mem",
		"system",
		"net",
		"netstat",
		"disk",
		"diskio",
		"kernel",
		"processes",
		"swap",
		"elasticsearch_breakers",
		"elasticsearch_cluster_health",
		"elasticsearch_clusterstats_indices",
		"elasticsearch_clusterstats_nodes",
		"elasticsearch_fs",
		"elasticsearch_http",
		"elasticsearch_indices",
		"elasticsearch_jvm",
		"elasticsearch_os",
		"elasticsearch_process",
		"elasticsearch_thread_pool",
		"elasticsearch_transport",
	}

	switch *usecase {
	case "today":
		log.Println("pulling todays data, --days parameter will be ignored")
		from, to, delta = today()
		cycles = 1
	case "yesterday":
		from, to, delta = midnight(*days)
		cycles = *days
	default:
		from, delta = custom(*days, *usecase)
		to = *usecase
		cycles = *days
	}

	d, err := InitData(*split, cycles, delta, *bSize, *anon)
	if err != nil {
		log.Fatal(err)
	}

	for i, m := range measurements {
		log.Println("Main: Starting query", i, "for", m)
		d.getDataFromInflux(m, from, to)
	}
	d.flush2csv()
}

from scrapy import Spider, Request
from scrapy.selector import Selector
from bikes.items import BikesItem


class BikesSpider(Spider):
	name = "bikes_spider"
	allowed_urls = ['https://bikeindex.org/']
	start_urls = ['https://bikeindex.org/bikes?distance=100&location=null&page=%s&serial=&stolenness=stolen' %str(i) for i in range (1,3710)] #start with 10 for now

	def parse(self, response):
		rows = response.xpath('//ul[@class = "bike-boxes"]/li')
		url_list = rows.xpath('./div/h5/a/@href').extract()
		
		for url in url_list:
			yield Request(url, callback = self.parse_url)

	def parse_url(self, response):

		Serial = response.xpath('//div[@class = "show-bike-details"]/ul/li[span[. = "Serial:"]]/text()').extract_first()
		Manufacturer = response.xpath('//div[@class = "show-bike-details"]/ul/li[span[. = "Manufacturer:"]]/text()').extract_first()
		Model = response.xpath('//div[@class = "show-bike-details"]/ul/li[span[. = "Model:"]]/text()').extract_first()
		Year = response.xpath('//div[@class = "show-bike-details"]/ul/li[span[. = "Year:"]]/text()').extract_first()
		Color = response.xpath('//div[@class = "show-bike-details"]/ul/li[span[. = "Primary colors:"]]/text()').extract_first()
		Size = response.xpath('//div[@class = "show-bike-details"]/ul/li[span[. = "Frame size:"]]/text()').extract_first()
		Material = response.xpath('//div[@class = "show-bike-details"]/ul/li[span[. = "Frame Material:"]]/text()').extract_first()
		Location = response.xpath('//div[@class = "col-md-4"]/ul/li[span[. = "Location"]]/text()').extract_first()
		Lock_desc = response.xpath('//div[@class = "col-md-4"]/ul/li[span[. = "Locking description"]]/text()').extract_first()
		Lock_circ = response.xpath('//div[@class = "col-md-4"]/ul/li[span[. = "Locking circumvented"]]/text()').extract_first()
		Date = response.xpath('//div[@class = "col-md-4"]/ul/li[span[. = "Date stolen"]]/text()').extract_first()

		if '\nDescription of incident\n' in response.xpath('//div[@class = "show-bike-details"]/h3/text()').extract():
			index = response.xpath('//div[@class = "show-bike-details"]/h3/text()').extract().index('\nDescription of incident\n')
			Incident = response.xpath('//div[@class = "show-bike-details"]/p/text()').extract()[index-1].strip('\n')
		else:
			Incident = "NA"

		
		item = BikesItem()
		item['Serial'] = Serial
		item['Manufacturer'] = Manufacturer
		item['Model'] = Model
		item['Year'] = Year
		item['Color'] = Color
		item['Size'] = Size
		item['Material'] = Material
		item['Location'] = Location
		item['Lock_desc'] = Lock_desc
		item['Lock_circ'] = Lock_circ
		item['Date'] = Date
		item['Incident'] = Incident

		yield item
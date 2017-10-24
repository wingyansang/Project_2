# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class BikesItem(scrapy.Item):
	Serial = scrapy.Field()
	Model = scrapy.Field()
	Color = scrapy.Field()
	Manufacturer = scrapy.Field()
	Year = scrapy.Field()
	Size = scrapy.Field()
	Location = scrapy.Field()
	Lock_desc = scrapy.Field()
	Lock_circ = scrapy.Field()
	Date = scrapy.Field()
	Material = scrapy.Field()
	Incident = scrapy.Field()


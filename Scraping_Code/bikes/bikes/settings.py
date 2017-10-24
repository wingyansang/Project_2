# -*- coding: utf-8 -*-

BOT_NAME = 'bikes'

SPIDER_MODULES = ['bikes.spiders']
NEWSPIDER_MODULE = 'bikes.spiders'

DOWNLOAD_DELAY = .25

ITEM_PIPELINES = {'bikes.pipelines.ValidateItemPipeline': 100,
                  'bikes.pipelines.WriteItemPipeline': 200}
import scrapy
import re


class RozetkaScrapper(scrapy.Spider):
    name = 'reviews'
    start_urls = [
        'https://bt.rozetka.com.ua/electric_kettles/c80160/filter/',
        'https://bt.rozetka.com.ua/coffee_machines/c80164/filter/',
        'https://bt.rozetka.com.ua/blenders/c80155/filter/',
        'https://bt.rozetka.com.ua/meat_choppers/c80176/filter/',
        'https://bt.rozetka.com.ua/kitchen_machines/c80163/filter/',
        'https://bt.rozetka.com.ua/squeezers/c80153/',
        'https://bt.rozetka.com.ua/grills/c81235/filter/',
        'https://bt.rozetka.com.ua/sandwich_makers/c80180/',
        'https://bt.rozetka.com.ua/mixers/c80156/',
        'https://bt.rozetka.com.ua/washing_machines/c80124/filter/',
        'https://bt.rozetka.com.ua/refrigerators/c80125/filter/',
        'https://bt.rozetka.com.ua/ovens/c80141/filter/',
        'https://bt.rozetka.com.ua/freezers/c80203/',
        'https://bt.rozetka.com.ua/cookers/c80122/filter/',
        'https://bt.rozetka.com.ua/extractor_fans/c80140/filter/',
        'https://bt.rozetka.com.ua/dishwashers/c80123/',
        'https://rozetka.com.ua/notebooks/c80004/filter/',
        'https://rozetka.com.ua/tablets/c130309/filter/',
        'https://rozetka.com.ua/photo/c80001/filter/',
        'https://rozetka.com.ua/headphones/c80027/',
        'https://rozetka.com.ua/mp3-pmp/c80016/',
        'https://rozetka.com.ua/video/c80002/',
        'https://rozetka.com.ua/e-books/c80023/',
        'https://hard.rozetka.com.ua/mouses/c80172/',
        'https://hard.rozetka.com.ua/monitors/c80089/',
        'https://hard.rozetka.com.ua/ssd/c80109/',
        'https://rozetka.com.ua/printers-mfu/c80007/filter/',
        'https://hard.rozetka.com.ua/psu/c80086/',
    ]

    num_reviews_regexp = '(\d+)\s.+'
    html_tags_regexp = re.compile(r'<[^>]+>')

    custom_settings = {
        'FEED_EXPORT_ENCODING': 'utf-8',
        'CONCURRENT_REQUESTS': 100,
        'REACTOR_THREADPOOL_MAXSIZE': 10
    }

    def parse(self, response):
        nav = list(response.css('nav.paginator-catalog.pos-fix > ul > li'))
        if nav:
            last_page = nav[-1]
            num_pages = last_page.css('span::text').extract_first()
            num_pages = int(num_pages)

            self.logger.info("Num pages detected".format(num_pages))

            for page in range(1, num_pages + 1):
                url = 'page={}'.format(page)
                yield scrapy.Request(response.urljoin(url), callback=self.parse_page)
        else:
            yield scrapy.Request(response.request.url, callback=self.parse_page)

    def parse_page(self, response):
        goods = response.css('div#catalog_goods_block div.g-i-tile.g-i-tile-catalog')
        for g in goods:
            num_reviews_text = g\
                .css('div[name="prices_active_element_original"] span.g-rating-reviews::text')\
                .extract_first()

            if not num_reviews_text or not num_reviews_text.strip():
                continue

            num_reviews_text = num_reviews_text.strip()
            self.logger.info('Num reviews {} for {}'.format(num_reviews_text,
                                                            response.request.url))
            num_reviews = re.search(self.num_reviews_regexp, num_reviews_text).group(1)

            if num_reviews == 0:
                continue

            reviews_link = g\
                .css('div[name="prices_active_element_original"] a.g-rating-reviews-link::attr(href)')\
                .extract_first()

            yield scrapy.Request(reviews_link, callback=self.parse_review_pages)

    def parse_review_pages(self, response):
        nav = list(response.css('nav.paginator-comments > ul > li'))
        if nav:
            last_page = list(nav)[-1]
            num_pages = last_page.css('span::text').extract_first()
            num_pages = int(num_pages)

            self.logger.info('Num comment pages detected {} in {}'.format(num_pages,
                                                                          response.request.url))

            for page in range(1, num_pages + 1):
                url = 'page={}'.format(page)
                yield scrapy.Request(response.urljoin(url), callback=self.parse_reviews)
        else:
            yield scrapy.Request(response.request.url, callback=self.parse_reviews)

    def parse_reviews(self, response):
        reviews = response.css('div#comments > article.pp-review-i')

        for review in reviews:
            stars = review.css('div.pp-review-inner div.g-rating-b > span > span::attr(content)')\
                .extract_first()
            if not stars:
                continue
            paragraphs = []
            review_text = review.css('div.pp-review-inner div.pp-review-text > div.pp-review-text-i')
            for p in review_text:
                txt = p.xpath('string()').extract_first()
                if txt:
                    clean = txt.replace('\n', ' ').replace(u'\xa0', u' ').strip()
                    paragraphs.append(clean)

            yield {'stars': stars,
                   'text': paragraphs}


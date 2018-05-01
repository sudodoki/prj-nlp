import scrapy
import re
from twisted.internet import reactor
import scrapy
from scrapy.crawler import CrawlerRunner
from scrapy.utils.log import configure_logging
from scrapy.crawler import CrawlerProcess

mapping = {'nm0000095': 'Woody Allen',
 'nm0000142': 'Clint Eastwood',
 'nm0000165': 'Ron Howard',
 'nm0000217': 'Martin Scorsese',
 'nm0000229': 'Steven Spielberg',
 'nm0000233': 'Quentin Tarantino',
 'nm0000265': 'Robert Altman',
 'nm0000318': 'Tim Burton',
 'nm0000343': 'David Cronenberg',
 'nm0000399': 'David Fincher',
 'nm0000436': 'Curtis Hanson',
 'nm0000464': 'Jim Jarmusch',
 'nm0000466': 'Jean-Pierre Jeunet',
 'nm0000487': 'Ang Lee',
 'nm0000490': 'Spike Lee',
 'nm0000500': 'Richard Linklater',
 'nm0000517': 'Terrence Malick',
 'nm0000519': 'David Mamet',
 'nm0000520': 'Michael Mann',
 'nm0000591': 'Roman Polanski',
 'nm0000600': 'Sam Raimi',
 'nm0000631': 'Ridley Scott',
 'nm0000709': 'Robert Zemeckis',
 'nm0000759': 'Paul Thomas Anderson',
 'nm0000776': 'Michael Apted',
 'nm0000801': 'Olivier Assayas',
 'nm0000876': 'Noah Baumbach',
 'nm0000965': 'Danny Boyle',
 'nm0001031': 'Claude Chabrol',
 'nm0001129': 'Jonathan Demme',
 'nm0001241': 'Stephen Frears',
 'nm0001348': 'Werner Herzog',
 'nm0001392': 'Peter Jackson',
 'nm0001403': 'Neil Jordan',
 'nm0001554': 'Errol Morris',
 'nm0001565': 'Mike Newell',
 'nm0001741': 'Bryan Singer',
 'nm0001752': 'Steven Soderbergh',
 'nm0001754': 'Todd Solondz',
 'nm0001885': 'Lars von Trier',
 'nm0002083': 'Carl Franklin',
 'nm0003506': 'James Mangold',
 'nm0004716': 'Darren Aronofsky',
 'nm0005139': 'Mike Leigh',
 'nm0005222': 'Sam Mendes',
 'nm0006960': 'John Madden',
 'nm0015359': 'Fatih Akin',
 'nm0021899': 'Michael Almereyda',
 'nm0027572': 'Wes Anderson',
 'nm0070159': 'Jafar Panahi',
 'nm0081540': 'Susanne Bier',
 'nm0106924': 'Catherine Breillat',
 'nm0174374': 'Bill Condon',
 'nm0201094': 'Jean-Pierre Dardenne',
 'nm0208343': 'Raymond De Felitta',
 'nm0219136': 'Claire Denis',
 'nm0225269': 'Kirby Dick',
 'nm0241622': 'Bruno Dumont',
 'nm0269463': 'Jon Favreau',
 'nm0276349': 'Shane Meadows',
 'nm0284774': 'Anne Fontaine',
 'nm0288144': 'Alastair Fothergill',
 'nm0305017': 'Liz Garbus',
 'nm0316795': 'Alex Gibney',
 'nm0327273': 'Michel Gondry',
 'nm0337773': 'David Gordon Green',
 'nm0339030': 'Paul Greengrass',
 'nm0359734': 'Michael Haneke',
 'nm0453580': 'Ki-duk Kim',
 'nm0466153': 'Hirokazu Koreeda',
 'nm0475905': 'Kiyoshi Kurosawa',
 'nm0496312': 'Patrice Leconte',
 'nm0510731': 'Doug Liman',
 'nm0516360': 'Ken Loach',
 'nm0531817': 'Kevin Macdonald',
 'nm0533284': 'David Mackenzie',
 'nm0585011': 'Roger Michell',
 'nm0601618': 'Michael Moore',
 'nm0634240': 'Christopher Nolan',
 'nm0661791': 'Chan-wook Park',
 'nm0716347': 'Nicolas Winding Refn',
 'nm0718646': 'Jason Reitman',
 'nm0751102': 'David O. Russell',
 'nm0758574': 'Walter Salles',
 'nm0771054': 'Lone Scherfig',
 'nm0782430': 'Ulrich Seidl',
 'nm0864775': 'Johnnie To',
 'nm0868219': 'Guillermo del Toro',
 'nm0898288': 'Denis Villeneuve',
 'nm0935863': 'Michael Winterbottom',
 'nm0939182': 'Kar-Wai Wong',
 'nm0946733': 'David Yates',
 'nm0955443': 'Yimou Zhang',
 'nm1296554': 'Ben Wheatley'}

class ImdbSpider(scrapy.Spider):
    name = 'imdbspider'    
    start_urls = ['http://www.imdb.com/name/' + id for id in mapping.keys()]
    

    def processContent(self, content):
        return re.sub("[\n\r\t]{1,}", "\n", ''.join(content)).strip()

    def parse(self, response):        
        films = [] 
        for film in  response.xpath('//div[@id="filmo-head-director"]/following-sibling::div[1]/div'):            
            films.append( {
                'year': film.css('div .year_column::text').extract_first().strip(),
                'name' : film.css("div b a::text").extract_first().strip()
                })
        yield {
            'name' : mapping[response.request.url[-10:-1]],
            'films' : films
        }

process = CrawlerProcess({
    'FEED_FROMAT' : 'jl',
    'FEED_URI' : 'result.json',
    'FEED_EXPORT_ENCODING': 'utf-8'
})
process.crawl(ImdbSpider)
process.start()

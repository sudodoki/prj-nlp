# -*- coding: utf-8 -*-
import scrapy
import re
import spacy
from spacy.symbols import *

## \brief      Fill map "capital city name" -> "country name"
## Names were copied from https://en.wikipedia.org/wiki/List_of_national_capitals_in_alphabetical_order
##
def fill_database():
    capitals_db = {}
    capitals_db[u"Abu Dhabi"] = u"United Arab Emirates"
    capitals_db[u"Abuja"] = u"Nigeria"
    capitals_db[u"Accra"] = u"Ghana"
    capitals_db[u"Adamstown"] = u"Pitcairn"
    capitals_db[u"Addis Ababa"] = u"Ethiopia"
    capitals_db[u"Algiers"] = u"Algeria"
    capitals_db[u"Alofi"] = u"Niue"
    capitals_db[u"Amman"] = u"Jordan"
    capitals_db[u"Amsterdam"] = u"Netherlands"
    capitals_db[u"Andorra la Vella"] = u"Andorra"
    capitals_db[u"Ankara"] = u"Turkey"
    capitals_db[u"Antananarivo"] = u"Madagascar"
    capitals_db[u"Apia"] = u"Samoa"
    capitals_db[u"Ashgabat"] = u"Turkmenistan"
    capitals_db[u"Asmara"] = u"Eritrea"
    capitals_db[u"Astana"] = u"Kazakhstan"
    capitals_db[u"Asunción"] = u"Paraguay"
    capitals_db[u"Athens"] = u"Greece"
    capitals_db[u"Avarua"] = u"Cook Islands"
    capitals_db[u"Baghdad"] = u"Iraq"
    capitals_db[u"Baku"] = u"Azerbaijan"
    capitals_db[u"Bamako"] = u"Mali"
    capitals_db[u"Bandar Seri Begawan"] = u"Brunei"
    capitals_db[u"Bangkok"] = u"Thailand"
    capitals_db[u"Bangui"] = u"Central African Republic"
    capitals_db[u"Banjul"] = u"Gambia"
    capitals_db[u"Basseterre"] = u"Saint Kitts and Nevis"
    capitals_db[u"Beijing"] = u"China"
    capitals_db[u"Beirut"] = u"Lebanon"
    capitals_db[u"Belgrade"] = u"Serbia"
    capitals_db[u"Belmopan"] = u"Belize"
    capitals_db[u"Berlin"] = u"Germany"
    capitals_db[u"Bern"] = u"Switzerland"
    capitals_db[u"Bishkek"] = u"Kyrgyzstan"
    capitals_db[u"Bissau"] = u"Guinea-Bissau"
    capitals_db[u"Bogotá"] = u"Colombia"
    capitals_db[u"Brasília"] = u"Brazil"
    capitals_db[u"Bratislava"] = u"Slovakia"
    capitals_db[u"Brazzaville"] = u"Republic of the Congo"
    capitals_db[u"Bridgetown"] = u"Barbados"
    capitals_db[u"Brussels"] = u"Belgium"
    capitals_db[u"Bucharest"] = u"Romania"
    capitals_db[u"Budapest"] = u"Hungary"
    capitals_db[u"Buenos Aires"] = u"Argentina"
    capitals_db[u"Bujumbura"] = u"Burundi"
    capitals_db[u"Cairo"] = u"Egypt"
    capitals_db[u"Canberra"] = u"Australia"
    capitals_db[u"Caracas"] = u"Venezuela"
    capitals_db[u"Castries"] = u"Saint Lucia"
    capitals_db[u"Charlotte Amalie"] = u"United States Virgin Islands"
    capitals_db[u"Chișinău"] = u"Moldova"
    capitals_db[u"Cockburn Town"] = u"Turks and Caicos Islands"
    capitals_db[u"Conakry"] = u"Guinea"
    capitals_db[u"Copenhagen"] = u"Denmark"
    capitals_db[u"Dakar"] = u"Senegal"
    capitals_db[u"Damascus"] = u"Syria"
    capitals_db[u"Dhaka"] = u"Bangladesh"
    capitals_db[u"Dili"] = u"East Timor"
    capitals_db[u"Djibouti"] = u"Djibouti"
    capitals_db[u"Dodoma"] = u"Tanzania"
    capitals_db[u"Doha"] = u"Qatar"
    capitals_db[u"Douglas"] = u"Isle of Man"
    capitals_db[u"Dublin"] = u"Ireland"
    capitals_db[u"Dushanbe"] = u"Tajikistan"
    capitals_db[u"Edinburgh of the Seven Seas"] = u"Tristan da Cunha"
    capitals_db[u"El Aioun"] = u"Sahrawi Arab Democratic Republic"
    capitals_db[u"Tifariti"] = u"Sahrawi Arab Democratic Republic"
    capitals_db[u"Episkopi Cantonment"] = u"Akrotiri and Dhekelia"
    capitals_db[u"Flying Fish Cove"] = u"Christmas Island"
    capitals_db[u"Freetown"] = u"Sierra Leone"
    capitals_db[u"Funafuti"] = u"Tuvalu"
    capitals_db[u"Gaborone"] = u"Botswana"
    capitals_db[u"George Town"] = u"Cayman Islands"
    capitals_db[u"Georgetown"] = u"Ascension Island"
    capitals_db[u"Georgetown"] = u"Guyana"
    capitals_db[u"Gibraltar"] = u"Gibraltar"
    capitals_db[u"Guatemala City"] = u"Guatemala"
    capitals_db[u"Gustavia"] = u"Saint Barthélemy"
    capitals_db[u"Hagåtña"] = u"Guam"
    capitals_db[u"Hamilton"] = u"Bermuda"
    capitals_db[u"Hanga Roa"] = u"Easter Island"
    capitals_db[u"Hanoi"] = u"Vietnam"
    capitals_db[u"Harare"] = u"Zimbabwe"
    capitals_db[u"Hargeisa"] = u"Somaliland"
    capitals_db[u"Havana"] = u"Cuba"
    capitals_db[u"Helsinki"] = u"Finland"
    capitals_db[u"Hong Kong"] = u"Hong Kong"
    capitals_db[u"Honiara"] = u"Solomon Islands"
    capitals_db[u"Islamabad"] = u"Pakistan"
    capitals_db[u"Jakarta"] = u"Indonesia"
    capitals_db[u"Jamestown"] = u"Saint Helena"
    capitals_db[u"Jerusalem"] = u"Israel"
    capitals_db[u"Juba"] = u"South Sudan"
    capitals_db[u"Kabul"] = u"Afghanistan"
    capitals_db[u"Kampala"] = u"Uganda"
    capitals_db[u"Kathmandu"] = u"Nepal"
    capitals_db[u"Khartoum"] = u"Sudan"
    capitals_db[u"Kiev"] = u"Ukraine"
    capitals_db[u"Kigali"] = u"Rwanda"
    capitals_db[u"King Edward Point"] = u"South Georgia and the South Sandwich Islands"
    capitals_db[u"Kingston"] = u"Jamaica"
    capitals_db[u"Kingston"] = u"Norfolk Island"
    capitals_db[u"Kingstown"] = u"Saint Vincent and the Grenadines"
    capitals_db[u"Kinshasa"] = u"Democratic Republic of the Congo"
    capitals_db[u"Kuala Lumpur"] = u"Malaysia"
    capitals_db[u"Putrajaya"] = u"Malaysia"
    capitals_db[u"Kuwait City"] = u"Kuwait"
    capitals_db[u"Libreville"] = u"Gabon"
    capitals_db[u"Lilongwe"] = u"Malawi"
    capitals_db[u"Lima"] = u"Peru"
    capitals_db[u"Lisbon"] = u"Portugal"
    capitals_db[u"Ljubljana"] = u"Slovenia"
    capitals_db[u"Lomé"] = u"Togo   "
    capitals_db[u"London"] = u"United Kingdom"
    capitals_db[u"Luanda"] = u"Angola"
    capitals_db[u"Lusaka"] = u"Zambia"
    capitals_db[u"Luxembourg"] = u"Luxembourg"
    capitals_db[u"Madrid"] = u"Spain"
    capitals_db[u"Majuro"] = u"Marshall Islands"
    capitals_db[u"Malabo"] = u"Equatorial Guinea"
    capitals_db[u"Malé"] = u"Maldives   "
    capitals_db[u"Managua"] = u"Nicaragua"
    capitals_db[u"Manama"] = u"Bahrain"
    capitals_db[u"Manila"] = u"Philippines"
    capitals_db[u"Maputo"] = u"Mozambique"
    capitals_db[u"Marigot"] = u"Saint Martin"
    capitals_db[u"Maseru"] = u"Lesotho"
    capitals_db[u"Mata-Utu"] = u"Wallis and Futuna"
    capitals_db[u"Mbabane"] = u"Swaziland"
    capitals_db[u"Lobamba"] = u"Swaziland"
    capitals_db[u"Mexico City"] = u"Mexico"
    capitals_db[u"Minsk"] = u"Belarus"
    capitals_db[u"Mogadishu"] = u"Somalia"
    capitals_db[u"Monaco"] = u"Monaco"
    capitals_db[u"Monrovia"] = u"Liberia"
    capitals_db[u"Montevideo"] = u"Uruguay"
    capitals_db[u"Moroni"] = u"Comoros"
    capitals_db[u"Moscow"] = u"Russia"
    capitals_db[u"Muscat"] = u"Oman"
    capitals_db[u"Nairobi"] = u"Kenya"
    capitals_db[u"Nassau"] = u"Bahamas"
    capitals_db[u"Naypyidaw"] = u"Myanmar"
    capitals_db[u"N'Djamena"] = u"Chad"
    capitals_db[u"New Delhi"] = u"India"
    capitals_db[u"Ngerulmud"] = u"Palau"
    capitals_db[u"Niamey"] = u"Niger"
    capitals_db[u"Nicosia"] = u"Cyprus"
    capitals_db[u"Nicosia"] = u"Northern Cyprus"
    capitals_db[u"Nouakchott"] = u"Mauritania"
    capitals_db[u"Nouméa"] = u"New Caledonia"
    capitals_db[u"Nukuʻalofa"] = u"Tonga"
    capitals_db[u"Nuuk"] = u"Greenland"
    capitals_db[u"Oranjestad"] = u"Aruba"
    capitals_db[u"Oslo"] = u"Norway"
    capitals_db[u"Ottawa"] = u"Canada"
    capitals_db[u"Ouagadougou"] = u"Burkina Faso"
    capitals_db[u"Pago Pago"] = u"American Samoa"
    capitals_db[u"Palikir"] = u"Federated States of Micronesia"
    capitals_db[u"Panama City"] = u"Panama"
    capitals_db[u"Papeete"] = u"French Polynesia"
    capitals_db[u"Paramaribo"] = u"Suriname"
    capitals_db[u"Paris"] = u"France"
    capitals_db[u"Philipsburg"] = u"Sint Maarten"
    capitals_db[u"Phnom Penh"] = u"Cambodia"
    capitals_db[u"Podgorica"] = u"Montenegro"
    capitals_db[u"Cetinje"] = u"Montenegro"
    capitals_db[u"Port Louis"] = u"Mauritius"
    capitals_db[u"Port Moresby"] = u"Papua New Guinea"
    capitals_db[u"Port Vila"] = u"Vanuatu"
    capitals_db[u"Port-au-Prince"] = u"Haiti"
    capitals_db[u"Port of Spain"] = u"Trinidad and Tobago"
    capitals_db[u"Porto-Novo"] = u"Benin"
    capitals_db[u"Cotonou (de facto)"] = u"Benin"
    capitals_db[u"Prague"] = u"Czech Republic"
    capitals_db[u"Praia"] = u"Cape Verde"
    capitals_db[u"Pretoria"] = u"South Africa"
    capitals_db[u"Bloemfontein"] = u"South Africa"
    capitals_db[u"Cape Town"] = u"South Africa"
    capitals_db[u"Pristina"] = u"Kosovo"
    capitals_db[u"Pyongyang"] = u"North Korea"
    capitals_db[u"Quito"] = u"Ecuador"
    capitals_db[u"Rabat"] = u"Morocco"
    capitals_db[u"Reykjavík"] = u"Iceland"
    capitals_db[u"Riga"] = u"Latvia"
    capitals_db[u"Riyadh"] = u"Saudi Arabia"
    capitals_db[u"Road Town"] = u"British Virgin Islands"
    capitals_db[u"Rome"] = u"Italy"
    capitals_db[u"Roseau"] = u"Dominica"
    capitals_db[u"Saipan"] = u"Northern Mariana Islands"
    capitals_db[u"San José"] = u"Costa Rica "
    capitals_db[u"San Juan"] = u"Puerto Rico"
    capitals_db[u"San Marino"] = u"San Marino"
    capitals_db[u"San Salvador"] = u"El Salvador"
    capitals_db[u"Sana'a "] = u"Yemen"
    capitals_db[u"Aden"] = u"Yemen"
    capitals_db[u"Santiago"] = u"Chile"
    capitals_db[u"Santo Domingo"] = u"Dominican Republic"
    capitals_db[u"São Tomé"] = u"São Tomé and Príncipe  "
    capitals_db[u"Sarajevo"] = u"Bosnia and Herzegovina"
    capitals_db[u"Seoul"] = u"South Korea"
    capitals_db[u"Singapore"] = u"Singapore"
    capitals_db[u"Skopje"] = u"Republic of Macedonia"
    capitals_db[u"Sofia"] = u"Bulgaria"
    capitals_db[u"Sri Jayawardenepura Kotte"] = u"Sri Lanka"
    capitals_db[u"Colombo"] = u"Sri Lanka"
    capitals_db[u"St. George's"] = u"Grenada"
    capitals_db[u"St. Helier"] = u"Jersey"
    capitals_db[u"St. John's"] = u"Antigua and Barbuda"
    capitals_db[u"St. Peter Port"] = u"Guernsey"
    capitals_db[u"St. Pierre"] = u"Saint Pierre and Miquelon"
    capitals_db[u"Stanley"] = u"Falkland Islands"
    capitals_db[u"Stepanakert"] = u"Artsakh"
    capitals_db[u"Stockholm"] = u"Sweden"
    capitals_db[u"Sucre"] = u"Bolivia"
    capitals_db[u"La Paz"] = u"Bolivia"
    capitals_db[u"Sukhumi"] = u"Abkhazia"
    capitals_db[u"Suva"] = u"Fiji"
    capitals_db[u"Taipei"] = u"Taiwan"
    capitals_db[u"Tallinn"] = u"Estonia"
    capitals_db[u"Tarawa"] = u"Kiribati"
    capitals_db[u"Tashkent"] = u"Uzbekistan"
    capitals_db[u"Tbilisi"] = u"Georgia"
    capitals_db[u"Kutaisi"] = u"Georgia"
    capitals_db[u"Tegucigalpa"] = u"Honduras"
    capitals_db[u"Tehran"] = u"Iran"
    capitals_db[u"Thimphu"] = u"Bhutan"
    capitals_db[u"Tirana"] = u"Albania"
    capitals_db[u"Tiraspol"] = u"Transnistria"
    capitals_db[u"Tokyo"] = u"Japan"
    capitals_db[u"Tórshavn"] = u"Faroe Islands"
    capitals_db[u"Tripoli"] = u"Libya"
    capitals_db[u"Tskhinvali"] = u"South Ossetia"
    capitals_db[u"Tunis"] = u"Tunisia"
    capitals_db[u"Ulaanbaatar"] = u"Mongolia"
    capitals_db[u"Vaduz"] = u"Liechtenstein"
    capitals_db[u"Valletta"] = u"Malta"
    capitals_db[u"The Valley"] = u"Anguilla"
    capitals_db[u"Vatican City"] = u"Vatican City"
    capitals_db[u"Victoria"] = u"Seychelles"
    capitals_db[u"Vienna"] = u"Austria"
    capitals_db[u"Vientiane"] = u"Laos"
    capitals_db[u"Vilnius"] = u"Lithuania"
    capitals_db[u"Warsaw"] = u"Poland"
    capitals_db[u"Washington, D.C."] = u"United States"
    capitals_db[u"Wellington"] = u"New Zealand"
    capitals_db[u"West Island"] = u"Cocos Islands"
    capitals_db[u"Willemstad"] = u"Curaçao"
    capitals_db[u"Windhoek"] = u"Namibia"
    capitals_db[u"Yamoussoukro"] = u"Ivory Coast"
    capitals_db[u"Abidjan"] = u"Ivory Coast"
    capitals_db[u"Yaoundé"] = u"Cameroon"
    capitals_db[u"Yaren"] = u"Nauru"
    capitals_db[u"Yerevan"] = u"Armenia"
    capitals_db[u"Zagreb"] = u"Croatia"
    return capitals_db

## \brief      Assume wikipedia urls based on city name.
## Names of big cities are used as test set.
## Cities were copied from https://en.wikipedia.org/wiki/List_of_largest_cities
##
def fill_urls_list():
    big_cities = [u'Chongqing', u'Shanghai', u'Delhi', u'Beijing', u'Mumbai', u'Lagos', u'Karachi', u'Guangzhou',\
     u'Istanbul', u'Tokyo', u'Tianjin', u'Moscow', u'Dhaka', u'São Paulo', u'Lahore', u'Cairo', u'Seoul', u'Kinshasa', \
     u'Jakarta', u'Wenzhou', u'Mexico City', u'Lima', u'London', u'Bengaluru', u'Chennai', u'New York City', u'Hyderabad',\
      u'Shenzhen', u'Bangkok', u'Suzhou', u'Nanjing', u'Dongguan', u'Tehran', u'Quanzhou', u'Shenyang', u'Ho Chi Minh City', \
      u'Hong Kong', u'Baghdad', u'Fuzhou', u'Changsha', u'Wuhan', u'Hanoi', u'Rio de Janeiro', u'Qingdao', u'Foshan', \
      u'Zunyi', u'Santiago', u'Riyadh', u'Ahmedabad', u'Singapore', u'Shantou', u'Ankara', u'Yangon', u'Saint Petersburg', \
      u'Casablanca', u'Sydney', u'Abidjan', u'Chengdu', u'Melbourne', u'Alexandria', u'Kolkata', u'Surat', u'Johannesburg', \
      u'Dar es Salaam', u'Shijiazhuang', u'Harbin', u'Giza', u'İzmir', u'Zhengzhou', u'New Taipei City', u'Los Angeles', \
      u'Changchun', u'Cape Town', u'Yokohama', u'Khartoum', u'Guayaquil', u'Hangzhou', u'Xiamen', u'Berlin', u'Busan', \
      u'Ningbo', u'Jeddah', u'Durban', u'Algiers', u'Kabul', u'Hefei', u'Mashhad', u'Pyongyang', u'Madrid', u'Faisalabad', \
      u'Baku', u'Tangshan', u'Ekurhuleni', u'Nairobi', u'Zhongshan', u'Pune', u'Addis Ababa', u'Jaipur', u'Buenos Aires', \
      u'Incheon', u'Quezon City', u'Kiev', u'Salvador', u'Rome', u'Dubai', u'Luanda', u'Lucknow', u'Kaohsiung', u'Kanpur', \
      u'Surabaya', u'Taichung', u'Basra', u'Toronto', u'Taipei', u'Chicago', u'Osaka', u'Quito', u'Chaozhou', u'Fortaleza', \
      u'Chittagong', u'Bandung', u'Managua', u'Brasília', u'Belo Horizonte', u'Daegu', u'Houston', u'Douala', u'Medellin', \
      u'Yaoundé', u'Nagpur', u'Cali', u'Tashkent', u'Nagoya', u'Isfahan', u'Phnom Penh', u'Paris', u'Ouagadougou', u'Lanzhou', \
      u'Kano', u'Dalian', u'Guatemala City', u'Havana', u'Rawalpindi', u'Medan', u'Accra', u'Visakhapatnam', u'Gujranwala',\
       u'Jinan', u'Karaj', u'Peshawar', u'Minsk', u'Caracas', u'Sapporo', u'Tainan', u'Bucharest', u'Curitiba', u'Shiraz', \
       u'Vienna', u'Brazzaville', u'Bhopal', u'Almaty', u'Hamburg', u'Manila', u'Kuala Lumpur', u'Maputo', u'Budapest', \
       u'Warsaw', u'Lusaka', u'Kathmandu', u'Tabriz', u'Hyderabad', u'Palembang', u'Tijuana', u'Patna', u'Montreal', \
       u'Davao City', u'Harare', u'Barcelona', u'Maracaibo', u'Caloocan', u'Philadelphia', u'Novosibirsk', u'Phoenix', \
       u'Bulawayo', u'Oran', u'Semarang', u'Recife', u'Kobe', u'Daejeon', u'Kampala', u'Kawasaki', u'Guadalajara', \
       u'Auckland', u'Vijayawada', u'Fukuoka', u'Kwangju', u'Porto Alegre', u'Kyoto', u'San Antonio', u'Santa Cruz de la Sierra', \
       u'Munich', u'Kharkiv', u'Yekaterinburg', u'San Diego', u'Barranquilla', u'Milan', u'Ibadan', u'Makassar', u'Prague', \
       u'Mandalay', u'Dallas', u'Montevideo', u'Sofia', u'Nizhny Novgorod', u'Abuja', u'Calgary', u'Saitama', u'Suwon', \
       u'Hiroshima', u'Rosario', u'Brisbane', u'Belgrade', u'Campinas', u'Ulsan', u'Omsk', u'Dakar', u'Abu Dhabi', u'Monterrey',\
       u'Tripoli', u'Rostov-on-Don', u'Fez', u'Birmingham', u'Yerevan', u'Cologne', u'Tunis', u'Astana', u'Islamabad']

    urls = []
    for city in big_cities:
        city = city.replace(' ', '_')
        city = city.replace('-', '_')

        url = 'https://en.wikipedia.org/wiki/' + city[0] + city[1:].lower()
        urls.append(url)

    return urls

## \brief      Check if pair (capital_name - country_name) presents in capital city database.
## Contract:
## If there is no country for capital_name in db -> the pair (capital_name - "") is accepted
## If there is no capital for country_name in db -> the pair (capital_name - country_name) is ignored
##
##
def check_in_database(capital_name, country_name, capitals_db):
    result = ''
    if len(country_name) > 0:
        result = capital_name + ' is capital of ' + country_name
    else:
        result = capital_name + ' is not a capital'

    if capitals_db.has_key(capital_name):
        if capitals_db[capital_name] == country_name:
            result += ' ->>>> True'
        else:
            result += ' ->>>> False'
    else:
        if len(country_name) > 0:
            result += ' ->>>> False'
        else:
            result += ' ->>>> True'

    if (len(country_name) <= 0) or (country_name in capitals_db.values()):
        print result


class CapitalsSpider(scrapy.Spider):
    name = 'capitals'
    allowed_domains = ['en.wikipedia.org']

    nlp = spacy.load('en')
    capitals_db = fill_database()


    ## \brief      main
    ##
    def start_requests(self):
        urls = fill_urls_list()
        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse_page)


    ## \brief     process content of a particular page
    ##
    ##
    def parse_page(self, response):
        title = response.css('title::text').extract()[0]
        title = title.replace(' - Wikipedia', '')
        main_text = response.css('p').extract()

        capital_info_found = False
        capital_name = ''
        country_name = ''
        for text in main_text:
            clean_text = re.sub('<.+?>', '', text)
            clean_text = re.sub('\[.+?\]', '', clean_text)
            clean_text = re.sub('\(.+?\)', '', clean_text)
            clean_text = clean_text.replace('\n', ' ')
            clean_text = clean_text.replace('\t', ' ')

            capital_info_found = self.find_capital_country_pair(clean_text)
            if capital_info_found == True:
                break

        if capital_info_found == False:
            check_in_database(title, '', self.capitals_db)


    ## \brief      rule to get smth like <City>-is-capital-of-<Country> or Capital-of-<Country>-is-<City>
    ##
    ##
    def find_capital_country_pair(self, text):
        if 'capital ' not in text.lower():
            return False

        capital_info_found = False
        text = text.encode('utf-8').decode('utf-8')
        doc = self.nlp(text)
        for sent in doc.sents:
            root_verb = sent.root
            if root_verb.pos != VERB:
                continue
            capital_name = ''
            country_name = ''
            for word in root_verb.children:
                if word.pos == PROPN:
                    try:
                        capital_names = list(x for x in doc.noun_chunks if x.root == word)[0]
                        capital_name = capital_names.text
                    except:
                        continue
                elif word.lemma_ == 'capital':
                    names = list(x for x in word.subtree if x.pos == PROPN and x.dep == pobj)
                    for chunk in doc.noun_chunks:
                        if chunk.root in names:
                            country_name = chunk.text
                            break
                elif 'capital' in list(x.lemma_ for x in word.conjuncts):
                    try:
                        capital_word = list(x for x in word.conjuncts if x.lemma_ == 'capital')[0]
                    except:
                        continue

                    names = list(x for x in capital_word.subtree if x.pos == PROPN and x.dep == pobj)
                    for chunk in doc.noun_chunks:
                        if chunk.root in names:
                            country_name = chunk.text
                            break

            if (len(capital_name) > 0) and (len(country_name) > 0):
                capital_info_found = True
                check_in_database(capital_name, country_name, self.capitals_db)
                break

        return capital_info_found







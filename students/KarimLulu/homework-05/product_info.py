def process_string(s):
    if isinstance(s, str):
        return s.strip()
    return ""

output = defaultdict(dict)
category = ""
c = 0
for row in rows:
    x = row.xpath('.//td')
    if len(x) == 1:
        temp = x.xpath('./text()').extract_first()
        temp = process_string(temp)
        if temp:
            category = temp
    elif len(x) == 2 and category:
        key = row.xpath('./td[1]/div')
        if key.xpath('./span'):
            prop = key.xpath('./span/text()').extract_first()
        else:
            prop = key.xpath('./text()').extract_first()
        prop = process_string(prop)
        if prop:
            value = row.xpath('./td[2]/div/span/text()').extract_first()
            value = process_string(value)
            output[category][prop] = value
            c+=1
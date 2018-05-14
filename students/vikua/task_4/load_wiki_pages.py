import os
import argparse
import json
import re

import aiohttp
import asyncio
import async_timeout


base_url = 'https://en.wikipedia.org/wiki'


async def fetch(session, url):
    async with async_timeout.timeout(60):
        async with session.get(url) as response:
            if response.status == 404 and url.endswith('_(film)'):
                return await fetch(session, url[:-7])
            elif response.status == 200:
                print('[{}] GET {}'.format(response.status, url))
                return await response.text()
            else:
                return ''


async def fetch_and_save(session, url, path):
    text = await fetch(session, url)
    if text:
        with open(path, 'w') as f:
            f.write(text)
    return True


async def get_pages(titles, output_dir):
    tasks = []
    async with aiohttp.ClientSession() as session:
        for t in titles:
            path = os.path.join(output_dir, '{}.html'.format(t.replace('/', '_')))
            url = '{}/{}'.format(base_url, t)
            task = asyncio.ensure_future(fetch_and_save(session, url, path))
            tasks.append(task)
        responses = await asyncio.gather(*tasks)
        print('Done', any(responses))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Fetch raw wiki pages')
    parser.add_argument('-i', dest='input_file', help='Input file with movie names')
    parser.add_argument('-o', dest='output_dir', help='Output dir, where to save wiki pages')

    args = parser.parse_args()

    with open(args.input_file, 'r') as f:
        content = json.loads(f.read())

    titles = []
    for k, v in content.items():
        titles.append(re.search('^.+\/(.+)$', v[0]).group(1))

    loop = asyncio.get_event_loop()
    future = asyncio.ensure_future(get_pages(titles, args.output_dir))
    loop.run_until_complete(future)

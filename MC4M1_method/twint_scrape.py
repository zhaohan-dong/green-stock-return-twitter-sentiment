
# This code will try to scrape twitter data through twint package

import asyncio
import aiohttp
import nest_asyncio
nest_asyncio.apply()
import twint
from datetime import date, timedelta

# Configure twint search (see https://github.com/twintproject/twint/wiki/Configuration for documentation)

twintq = 'global warming'

c = twint.Config()

c.Search = twintq
c.Store_csv = True
c.Output = "test.csv"
c.Hide_output = True

def daterange(start_date, end_date):
    for n in range(int((end_date - start_date).days) + 1):
        yield start_date + timedelta(n)

start_date = date(2020, 1, 1)
end_date = date(2020, 1, 2)

#for single_date in daterange(start_date, end_date):
#    str_date = single_date.strftime("%Y-%m-%d")
#    c.Since = str_date
#    c.Until = str_date
#    c.Output = "test.csv"
#    twint.run.Search(c)

twint.run.Search(c)

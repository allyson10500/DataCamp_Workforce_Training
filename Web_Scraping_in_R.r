library(rvest)

html_excerpt_raw <- '
<html> 
  <body> 
    <h1>Web scraping is cool</h1>
    <p>It involves writing code – be it R or Python.</p>
    <p><a href="https://datacamp.com">DataCamp</a> 
		has courses on it.</p>
  </body> 
</html>'
# Turn the raw excerpt into an HTML document R understands
html_excerpt <- read_html(html_excerpt_raw)
html_excerpt
# Print the HTML excerpt with the xml_structure() function
xml_structure(html_excerpt)

read_html(broken_html_raw)
xml_structure(read_html(broken_html_raw))

# Read in the corresponding HTML string
list_html <- read_html(list_raw_html)
# Extract the ol node
ol_node <- list_html %>% 
	html_element('ol')
# Extract and print the nodeset of all the children of ol_node
ol_node %>% html_children()

# Extract all the a nodes from the bulleted list
links <- hyperlink_raw_html %>% 
  read_html() %>%
  html_elements('li a') # 'ul a' is also correct!

# Extract the needed values for the data frame
domain_value = links %>% html_attr('href')
name_value = links %>% html_text()

# Construct a data frame
link_df <- tibble(
  domain = domain_value,
  name = name_value
)

link_df

# Extract the "clean" table into a data frame 
mountains <- mountains_html %>% 
  html_element("table#clean") %>% 
  html_table(header = TRUE)

mountains

# Extract the "dirty" table into a data frame
mountains <- mountains_html %>% 
  html_element("table#dirty") %>% 
  html_table(header = TRUE)

mountains

# Read in the HTML
languages_html <- read_html(languages_raw_html)
# Select the div and p tags and print their text
languages_html %>%
	html_elements('div, p') %>%
	html_text()

# Select the first div
structured_html %>%
  html_elements('#first')

# Select the last child of each p group
nested_html %>%
	html_elements('p:last-child')

# This time for real: Select only the last node of the p's wrapped by the div
nested_html  %>% 
	html_elements('p.text:last-child')

# Extract the text of all list elements
languages_html %>% 
	html_elements('li') %>% 
	html_text()

# Extract only the text of the computer languages (without the sub lists)
languages_html %>% 
	html_elements('ul#languages > li') %>% 
	html_text()

# Select the three divs with a simple selector
complicated_html %>%
	html_elements('div div')

# Select only the first code element in the second example
code_html %>% 
	html_elements('h2.second + code')

# Select all code elements in the second example
code_html %>% 
	html_elements('h2.second ~ code')

# Select all p elements
weather_html %>%
	html_elements(xpath = '//p')

# Select p elements with the second class
weather_html %>%
	html_elements(xpath = '//p[@class = "second"]')

# Select p elements that are children of "#third"
weather_html %>%
	html_elements(xpath = '//*[@id = "third"]/p')

# Select p elements with class "second" that are children of "#third"
weather_html %>%
	html_elements(xpath = '//*[@id = "third"]/p[@class = "second"]')

# Select all divs
weather_html %>% 
  html_elements(xpath = '//div')

# Select all divs
weather_html %>% 
  html_elements(xpath = '//div')

# Select all divs with p descendants
weather_html %>% 
  html_elements(xpath = '//div[p]')

# Select all divs
weather_html %>% 
  html_elements(xpath = '//div')

# Select all divs with p descendants
weather_html %>% 
  html_elements(xpath = '//div[p]')

# Select all divs with p descendants having the "third" class
weather_html %>% 
  html_elements(xpath = '//div[p[@class = "third"]]')

# Select the text of the second p in every div
rules_html %>% 
  html_elements(xpath = '//div/p[position() = 2]') %>%
  html_text()

# Select every p except the second from every div
rules_html %>% 
  html_elements(xpath = '//div/p[position() != 2]') %>%
  html_text()

# Select the text of the last three nodes of the second div
rules_html %>% 
  html_elements(xpath = '//div[position() = 2]/*[position() >= 2]') %>%
  html_text()

# Select only divs with one header and at least two paragraphs
forecast_html %>%
	html_elements(xpath = '//div[count(h2) = 1 and count(p) > 1]')

# Extract the data frame from the table using a known function from rvest
roles <- roles_html %>% 
  html_element(xpath = "//table") %>% 
  html_table()
# Print the contents of the role data frame
roles

# Extract the actors in the cells having class "actor"
actors <- roles_html %>% 
  html_elements(xpath = '//table//td[@class = "actor"]') %>%
  html_text()
actors

# Extract the roles in the cells having class "role"
roles <- roles_html %>% 
  html_elements(xpath = '//table//td[@class = "role"]/em') %>% 
  html_text()
roles

# Extract the actors in the cells having class "actor"
actors <- roles_html %>% 
  html_elements(xpath = '//table//td[@class = "actor"]') %>%
  html_text()
actors

# Extract the roles in the cells having class "role"
roles <- roles_html %>% 
  html_elements(xpath = '//table//td[@class = "role"]/em') %>% 
  html_text()
roles

# Extract the functions using the appropriate XPATH function
functions <- roles_html %>% 
  html_elements(xpath = '//table//td[@class = "role"]/text()') %>%
  html_text(trim = TRUE)
functions

# Create a new data frame from the extracted vectors
cast <- tibble(
  Actor = actors, 
  Role = roles, 
  Function = functions)

cast

# Select all li elements
programming_html %>%
	html_elements(xpath = '//li')

# Select all li elements
programming_html %>%
	html_elements(xpath = '//li') %>%
	# Select all em elements within li elements that have "twice" as text
	html_elements(xpath = 'em[text() = "twice"]')

# Select all li elements
programming_html %>%
	html_elements(xpath = '//li') %>%
	# Select all em elements within li elements that have "twice" as text
	html_elements(xpath = 'em[text() = "twice"]') %>%
	# Wander up the tree to select the parent of the em 
    html_elements(xpath = '..')

# Get the HTML document from Wikipedia using httr
wikipedia_response <- GET('https://en.wikipedia.org/wiki/Varigotti')
# Parse the response into an HTML doc
wikipedia_page <- content(wikipedia_response)

# Get the HTML document from Wikipedia using httr
wikipedia_response <- GET('https://en.wikipedia.org/wiki/Varigotti')
# Parse the response into an HTML doc
wikipedia_page <- content(wikipedia_response)

# Extract the elevation with XPATH
wikipedia_page %>% 
	html_elements(xpath = '//table//tr[position() = 9]/td') %>% 
	html_text()

response <- GET('https://en.wikipedia.org/wiki/Varigott')
# Print status code of inexistent page
status_code(response)

# Access https://httpbin.org/headers with httr
response <- GET('https://httpbin.org/headers')
# Print its content
content(response)

# Pass a custom user agent to a GET query to the mentioned URL
response <- GET('https://httpbin.org/user-agent', user_agent("A request from a DataCamp course on scraping"))
# Print the response content
content(response)

# Globally set the user agent to "A request from a DataCamp course on scraping"
set_config(add_headers(`User-Agent` = "A request from a DataCamp course on scraping"))
# Pass a custom user agent to a GET query to the mentioned URL
response <- GET('https://httpbin.org/user-agent')
# Print the response content
content(response)

# Define a throttled read_html() function with a delay of 0.5s
read_html_delayed <- slowly(read_html, 
                            rate = rate_delay(0.5))

# Define a throttled read_html() function with a delay of 0.5s
read_html_delayed <- slowly(read_html, 
                            rate = rate_delay(0.5))
# Construct a loop that goes over all page urls
for(page_url in mountain_wiki_pages){
  # Read in the html of each URL with the function defined above
  html <- read_html_delayed(page_url)
}

# Define a throttled read_html() function with a delay of 0.5s
read_html_delayed <- slowly(read_html, 
                            rate = rate_delay(0.5))
# Construct a loop that goes over all page urls
for(page_url in mountain_wiki_pages){
  # Read in the html of each URL with a delay of 0.5s
  html <- read_html_delayed(page_url)
  # Extract the name of the peak and its coordinates
  peak <- html %>% 
  	html_element("#firstHeading") %>% html_text()
  coords <- html %>% 
    html_element("#coordinates .geo-dms") %>% html_text()
  print(paste(peak, coords, sep = ": "))
}


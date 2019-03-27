project_name<- 'LianJia'
batch = 300
root_url = 'http://sh.lianjia.com/ershoufang'
url_regex<- paste0(root_url, '/sh[0-9]*\\.html')
anti_url_regex<- paste0(root_url, '/[a-z]+?/[a-z0-9]{5,15}$')

# Install packages

options(stringsAsFactors = FALSE)

pckg<- c('rstudioapi', 'stringr', 'rvest')

install.packages.loop<- function(package.name.arrary) {
  
  # Check if package installed
  is.installed <- function(mypkg){
    is.element(mypkg, installed.packages()[,1])
  }
  
  # Install package if not installed
  for(i in seq(length(package.name.arrary))) {
    if (!is.installed(package.name.arrary[i])){
      install.packages(package.name.arrary[i])
    }
  }
}

install.packages.loop(pckg)

require(rstudioapi)
require(stringr)
require(rvest)


# File management

dir_creation<- function(project_name){
  
  dir_path<- rstudioapi::getActiveDocumentContext()$path
  dir_path<- gsub(pattern = '^(.*)/.*\\.R$', replacement = '\\1', x = dir_path)
  
  setwd(dir = dir_path)
  
  if(!dir.exists(file.path(getwd(), project_name))){
    dir.create(path = file.path(getwd(), project_name))
  }
  
  setwd(file.path(getwd(), project_name))
  
  queue<<- paste0(project_name, '.queue.txt')
  finished<<- paste0(project_name, '.finished.txt')
  data_file<<- paste0(project_name, '.rawdata.txt')
  
  if(!file.exists(queue)){
    write.table(x = root_url, file = queue, col.names = F, row.names = F, quote = F)
  }
  
  if(!file.exists(finished)){
    write.table(x = NULL, file = finished, col.names = F, row.names = F, quote = F)
  }
  
  if(!file.exists(data_file)){
    write.table(x = t(c("URL", all_css_name, 'sys.time')), fileEncoding = 'utf-8', sep = '\t',file = data_file, col.names = F, row.names = F)
  }
}


# Data fetching functions

Sys.setlocale(locale = 'Chinese')

price = 'body > section > div.m-maininfo.content-wrapper.u-pb20 > aside > div.maininfo-price.maininfo-item > div.price-total > span.price-num'
area = 'body > section > div.m-maininfo.content-wrapper.u-pb20 > aside > ul.maininfo-main.maininfo-item > li.main-item.u-tr > p.u-fz20.u-bold'
facing = 'body > section > div.m-maininfo.content-wrapper.u-pb20 > aside > ul.maininfo-main.maininfo-item > li.main-item.u-tc > div > p.u-fz20.u-bold'
year = 'body > section > div.m-maininfo.content-wrapper.u-pb20 > aside > ul.maininfo-main.maininfo-item > li.main-item.u-tr > p.u-mt8.u-fz12'
xiaoqu = 'body > section > div.m-maininfo.content-wrapper.u-pb20 > aside > ul.maininfo-minor.maininfo-item > li:nth-child(4) > span:nth-child(2) > span > a:nth-child(1)'
address = 'body > section > div.m-maininfo.content-wrapper.u-pb20 > aside > ul.maininfo-minor.maininfo-item > li:nth-child(5) > span.item-cell.maininfo-estate-address'
carpark = '#js-baseinfo-header > div.content-main.module-tb > div:nth-child(2) > div.module-col.baseinfo-col3 > ul > li:nth-child(4) > span.item-cell.baseinfo-parking-spot'
huxing = 'body > section > div.m-maininfo.content-wrapper.u-pb20 > aside > ul.maininfo-main.maininfo-item > li:nth-child(1) > p.u-fz20.u-bold'
sales_reason = '#js-baseinfo-header > div.content-main.module-tb > div:nth-child(3) > div.module-col.baseinfo-col3 > ul > li:nth-child(1) > span:nth-child(2)'
last_trade = '#js-baseinfo-header > div.content-main.module-tb > div:nth-child(3) > div.module-col.baseinfo-col2 > ul > li:nth-child(1) > span:nth-child(2)'
nianxian = '#js-baseinfo-header > div.content-main.module-tb > div:nth-child(3) > div.module-col.baseinfo-col2 > ul > li:nth-child(2) > span:nth-child(2)'
last_7_days = 'div > p.record-7day-value'
total_views = 'div > p.u-grey-light'
tag = '#js-baseinfo-header > div.content-main.module-tb > div:nth-child(4) > div.module-col.baseinfo-colspan2 > ul'


all_css<- c(price, area, facing, year, xiaoqu, address, carpark, huxing, sales_reason, nianxian, last_trade, last_7_days, total_views, tag)
all_css_name<- c('price', 'area', 'facing', 'year', 'xiaoqu', 'address', 'carpark', 'huxing', 'sales_reason', 'nianxian','last_trade', 'last_7_days', 'total_views', 'tag')

var_string_conv<- function(variables){
  
  str_var<- c()
  for(var in variables){
    str_var<- c(str_var, deparse(substitute(var)))
  }
  
  return(str_var)
}

link_extract<- function(html_file) {
  
  urls<- unique(html_attr(html_nodes(html_file, 'a'), 'href'))
  valid_index_full<- grep(pattern = paste0(root_url, '.*'), x = urls)
  valid_index_short<- grep(pattern = paste0('^',gsub(pattern = '(http|https)://.*?(/.*)', replacement = '\\2', x = root_url), '.*'), x = urls)
  
  valid_urls<- paste0(gsub(pattern = '(.*)?/.*', replacement = '\\1', x = root_url),urls[valid_index_short])
  valid_urls<- c(valid_urls, gsub(pattern = paste0('^.*(',root_url, '.*)'), replacement = '\\1', x = urls[valid_index_full]))
  return(unique(valid_urls))
}


data_extract<- function(html_file) {
  
  the_list<- c()
  
  for(i in all_css){
    temp<- html_nodes(x = html_file, css = i) %>% html_text(trim = T)
    if(length(temp)==0){
      temp = 'NA'
    }
    temp<- iconv(temp, 'utf-8', 'gbk')
    temp<- str_replace_all(string = temp, pattern = '\n', replacement = '|')
    temp<- str_replace_all(string = temp, pattern = ' ', replacement = '')
    the_list<- c(the_list, temp)
  }
  
  return(the_list)
}


# The cralwer

spider<- function(queue, finished, n = 15){
  
  con_queue<- file(queue, open = 'r')
  con_finished<- file(finished, open = 'r')
  queue_list<- readLines(con_queue)
  finished_list<- readLines(con_finished)
  queue_list<- queue_list[!(queue_list %in% finished_list)]
  
  queue_list<- queue_list[!grepl(pattern = anti_url_regex, x = queue_list)]
  
  target<- queue_list[grep(pattern = url_regex, x = queue_list)]
  none_target<- queue_list[!queue_list %in% target]
  
  if(length(target)>0){
    if(length(target)>n){
      temp<- target[1:n]
    } else {
      temp<- target
    }
  } else {
    if(length(none_target)>n){
      temp<- none_target[1:n]
    } else {
      temp<- none_target
    }
  }
  
  #work
  target_data<- NULL
  new_urls<- NULL
  captured_url<- NULL
  
  for(temp_url in temp) {
    
    cat(paste0('... Crawling ', temp_url, '\n'))
    
    tryCatch({
      html_file<- read_html(x = temp_url, encoding = 'utf-8')
      
      new_urls<- c(new_urls, link_extract(html_file))
      
      if(grepl(pattern = url_regex, x = temp_url)){
        ram<- data_extract(html_file)
        ram<- c(temp_url, ram, Sys.time())
        target_data<- rbind(target_data, ram)
      }
      
      captured_url<- c(captured_url, temp_url)
      
    }, error = function(e) {cat('ERROR: ', conditionMessage(e), '\n')})
  }
  
  
  #CLEAN UP
  if(length(new_urls)>0){
    new_urls<- unique(new_urls)
  } else {
    new_urls<- NULL
  }
  
  if(length(target_data)==0){
    target_data<- NULL
  }
  
  if(length(captured_url)==0){
    captured_url<- NULL
  }
  
  finished_list<- unique(c(finished_list, captured_url))
  if(length(finished_list)==0){
    finished_list<- NULL
  }
  
  queue_list<- c(queue_list, new_urls)
  queue_list<- unique(queue_list[!queue_list %in% captured_url])
  queue_list<- queue_list[!(queue_list %in% finished_list)]
  if(length(queue_list)==0){
    queue_list<- NULL
  }

  #queue list
  if(!is.null(queue_list)){
    writeLines(text = queue_list, con = queue)
    # write.table(x = queue_list, file = queue, append = F, row.names = F, col.names = F, quote = F, fileEncoding = 'utf-8')
  }
  
  #finished list
  if(!is.null(finished_list)){
    writeLines(text = finished_list, con = finished)
    # write.table(x = finished_list, file = finished, append = F, row.names = F, col.names = F, quote = F, fileEncoding = 'utf-8')
  }
    
  #captured data
  if(!is.null(target_data)){
    write.table(x = target_data, file = data_file, append = T, row.names = F, col.names = F, sep = '\t', fileEncoding = 'utf-8')
  }
  
  url_num<- length(grep(pattern = url_regex, x = captured_url))
  initial_counting<<- initial_counting + url_num

  cat(' | Finished a batch with ', url_num, ifelse(url_num > 1, ' urls\n', ' url\n'), '| total of ', initial_counting, '\n',
      '|', capture.output(Sys.time()), '\n')
}

dir_creation(project_name = project_name)

con_data<- file(data_file, open = 'r')
initial_counting<- length(readLines(con_data))

repeat {
  spider(queue, finished, n = batch)
}





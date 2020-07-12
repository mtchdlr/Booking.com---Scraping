
library(robotstxt)
library(tidyverse)
#checking if we are allowed to scrape
#paths_allowed("https://www.yell.ge/restaurants.php?lan=eng#y=Tbilisi")
library(stringr)
library(dplyr)
library(rvest)
url_booking=("https://www.booking.com/searchresults.html?label=gen173nr-1DCAEoggI46AdIM1gEaFKIAQGYATG4ARfIAQzYAQPoAQH4AQKIAgGoAgO4Atzjm_YFwAIB&sid=3501e222cd28d5cae07e2e64ce96c956&sb=1&src=searchresults&src_elem=sb&error_url=https%3A%2F%2Fwww.booking.com%2Fsearchresults.html%3Flabel%3Dgen173nr-1DCAEoggI46AdIM1gEaFKIAQGYATG4ARfIAQzYAQPoAQH4AQKIAgGoAgO4Atzjm_YFwAIB%3Bsid%3D3501e222cd28d5cae07e2e64ce96c956%3Btmpl%3Dsearchresults%3Bcheckin_month%3D8%3Bcheckin_monthday%3D24%3Bcheckin_year%3D2020%3Bcheckout_month%3D8%3Bcheckout_monthday%3D31%3Bcheckout_year%3D2020%3Bcity%3D900047975%3Bclass_interval%3D1%3Bdest_id%3D900047975%3Bdest_type%3Dcity%3Bdtdisc%3D0%3Bfrom_sf%3D1%3Bgroup_adults%3D1%3Bgroup_children%3D0%3Binac%3D0%3Bindex_postcard%3D0%3Blabel_click%3Dundef%3Bno_rooms%3D1%3Boffset%3D0%3Bpostcard%3D0%3Broom1%3DA%3Bsb_price_type%3Dtotal%3Bshw_aparth%3D1%3Bslp_r_match%3D0%3Bsrc%3Dsearchresults%3Bsrc_elem%3Dsb%3Bsrpvid%3D846896e959990084%3Bss%3DTbilisi%3Bss_all%3D0%3Bssb%3Dempty%3Bsshis%3D0%3Bssne%3DTbilisi%3Bssne_untouched%3DTbilisi%3Btop_ufis%3D1%26%3B&ss=Tbilisi&is_ski_area=0&ssne=Tbilisi&ssne_untouched=Tbilisi&city=900047975&checkin_year=2020&checkin_month=8&checkin_monthday=24&checkout_year=2020&checkout_month=8&checkout_monthday=31&group_adults=2&group_children=0&no_rooms=1&from_sf=1")
response_booking=read_html(url_booking)
typeof(url_booking)

hotel=html_nodes(response_booking,css='span.sr-hotel__name')%>%html_text(trim=T)
hotel
price=html_nodes(response_booking,css="div.bui-price-display__value")%>%html_text(trim=T)
price
#rating=html_nodes(response_booking,css="div.bui-review-score__badge")%>%html_text(trim = T)
#rating
#rating_eval=html_nodes(response_booking,css="div.bui-review-score__title")%>%html_text(trim = T)
#rating_eval
#review_num=html_nodes(response_booking,css="div.bui-review-score__text")%>%html_text(trim = T)
#review_num

distance_from_center=html_nodes(response_booking,css="div.sr_card_address_line")%>%html_text(trim = T)
distance_from_center=str_remove_all(distance_from_center,'\n')
gsub(".*on map *(.*?) *Metro.*", "\\1", distance_from_center)
distance_from_center_true=str_remove(gsub("^.*?map","",str_remove_all(distance_from_center,'\n')),'Metro access')
distance_from_center_true

coordinates=html_nodes(response_booking,css="div.sr_card_address_line>a")%>%html_attr('data-coords')
coordinates


url_to_property=paste0('https://www.booking.com/',str_remove(html_nodes(response_booking,css="h3>a")%>%html_attr('href'),'\n/'))
url_to_property


thumbs_up_html=html_nodes(response_booking,css = 'div.sr-hotel__title-wrap>span')
thumbs_up_html
thumbs_up_true=str_replace(str_replace(str_detect(html_nodes(response_booking,css = 'div.sr-hotel__title-wrap>span'),'thumbs_up'),pattern = "TRUE",replacement = "Yes"),pattern = "FALSE",replacement = "No")
thumbs_up_true


overall_rating=html_nodes(response_booking,css = "div.bui-review-score__badge>span")
overall_rating


#THUMBS UP

library(webdriver)


install_phantomjs()
driver=run_phantomjs()
session=Session$new(port=driver$port)
session$go(url_booking)
pageWD=session$getSource()

length(thumbs_up)
thumbs_up=read_html(pageWD) %>% html_nodes(css = 'div.sr-hotel__title-wrap>span')
a=str_detect(thumbs_up, 'svg')
a=str_replace(str_replace(str_detect(read_html(pageWD) %>% html_nodes(css = 'div.sr-hotel__title-wrap>span'), 'svg'), pattern = 'TRUE', replacement = 'Yes'),pattern = 'FALSE', replacement = 'No')
a

a=a%>%
  unlist(a)%>%
  str_replace(pattern = 'TRUE', replacement = 'Yes')%>%
  str_replace(pattern = 'FALSE', replacement = 'No')
b=data.frame(a)
names(b)[names(b) == 'a']="Thumbs up"


#STARS
stars=html_nodes(response_booking,css = 'div.sr-hotel__title-wrap')
stars
true_stars=str_extract(html_nodes(response_booking,css = 'div.sr-hotel__title-wrap>span'), '[0-9]-star hotel')
true_stars


##BREAKFAST
#Bfast=html_nodes(response_booking,css = 'div.sr_item.sr_item_new')
#Bfast
#breakfast=str_replace(str_replace(str_detect(Bfast, 'Breakfast included'), pattern = 'TRUE', replacement = 'Yes'),pattern = 'FALSE', replacement = 'No')

#STUFFF
stuff=html_nodes(response_booking,css = 'div.roomPrice.roomPrice_flex.sr_discount')
str_detect(stuff, 'FREE cancellation')
str_detect(stuff, 'Breakfast included')
str_detect(stuff, 'No prepayment needed')
free_cancellation=str_replace(str_replace(str_detect(html_nodes(response_booking,css = 'div.roomPrice.roomPrice_flex.sr_discount'), 'FREE cancellation'), pattern = 'TRUE', replacement = 'FREE cancellation'),pattern = 'FALSE', replacement = 'No')
breakfast=str_replace(str_replace(str_detect(html_nodes(response_booking,css = 'div.roomPrice.roomPrice_flex.sr_discount'), 'Breakfast included'), pattern = 'TRUE', replacement = 'included'),pattern = 'FALSE', replacement = 'Not inlcuded')
No_prepayment_needed=str_replace(str_replace(str_detect(html_nodes(response_booking,css = 'div.roomPrice.roomPrice_flex.sr_discount'), 'No prepayment needed'), pattern = 'TRUE', replacement = 'No prepayment needed'),pattern = 'FALSE', replacement = 'Prepayment needed')
breakfast
No_prepayment_needed
# REVIEWS
rating_stuff=html_nodes(response_booking, css = 'div.reviewFloater')
rating_stuff_text=html_text(rating_stuff,trim = T)
rating_stuff_text

true_num_reviews=str_extract(gsub(".*score__text *(.*?) *</div>.*", "\\1", html_nodes(response_booking, css = 'div.reviewFloater')), '[0-9]+ reviews')
true_num_reviews

kind_words=gsub(".*[0-9] *(.*?) *[0-9]+ reviews.*", "\\1",html_nodes(response_booking, css = 'div.reviewFloater')%>%html_text(trim = T))
kind_words

rating_true=str_remove(gsub( " .*$", "", html_nodes(response_booking, css = 'div.reviewFloater')%>%html_text(trim = T) ), 'Just')
rating_true

#### HEAR GOES THE FUNCTION
booking_com_function=function(url_booking){
  response_booking=read_html(url_booking)
  hotel=unlist(html_nodes(response_booking,css='span.sr-hotel__name')%>%html_text(trim=T))
  #price=unlist(html_nodes(response_booking,css="div.bui-price-display__value")%>%html_text(trim=T))
  url_to_property=unlist(paste0('https://www.booking.com/',str_remove(html_nodes(response_booking,css="h3>a")%>%html_attr('href'),'\n/')))
  distance_from_center_true=unlist(str_remove(gsub("^.*?map","",str_remove_all(html_nodes(response_booking,css="div.sr_card_address_line")%>%html_text(trim = T),'\n')),'Metro access'))
  coordinates=unlist(html_nodes(response_booking,css="div.sr_card_address_line>a")%>%html_attr('data-coords'))
  #free_cancellation=unlist(str_replace(str_replace(str_detect(html_nodes(response_booking,css = 'div.roomPrice.roomPrice_flex.sr_discount'), 'FREE cancellation'), pattern = 'TRUE', replacement = 'FREE cancellation'),pattern = 'FALSE', replacement = 'No'))
  #breakfast=unlist(str_replace(str_replace(str_detect(html_nodes(response_booking,css = 'div.roomPrice.roomPrice_flex.sr_discount'), 'Breakfast included'), pattern = 'TRUE', replacement = 'included'),pattern = 'FALSE', replacement = 'Not inlcuded'))
  #No_prepayment_needed=unlist(str_replace(str_replace(str_detect(html_nodes(response_booking,css = 'div.roomPrice.roomPrice_flex.sr_discount'), 'No prepayment needed'), pattern = 'TRUE', replacement = 'No prepayment needed'),pattern = 'FALSE', replacement = 'Prepayment needed'))
  true_stars=unlist(str_extract(html_nodes(response_booking,css = 'div.sr-hotel__title-wrap>span.sr-hotel__title-badges'), '[0-9]-star hotel'))
  true_num_reviews=unlist(str_extract(gsub(".*score__text *(.*?) *</div>.*", "\\1", html_nodes(response_booking, css = 'div.reviewFloater')), '[0-9]+ reviews'))
  kind_words=unlist(gsub(".*[0-9] *(.*?) *[0-9]+ reviews.*", "\\1",html_nodes(response_booking, css = 'div.reviewFloater')%>%html_text(trim = T)))
  rating_true=unlist(str_remove(gsub( " .*$", "", html_nodes(response_booking, css = 'div.reviewFloater')%>%html_text(trim = T) ), 'Just'))
  thumbs_up_true=unlist(str_replace(str_replace(str_detect(html_nodes(response_booking,css = 'div.sr-hotel__title-wrap>span'),'thumbs_up'),pattern = "TRUE",replacement = "Yes"),pattern = "FALSE",replacement = "No"))
  output=list(Hotel=hotel, hyperlink=url_to_property, Price=price, Distance_from_centre=distance_from_center_true, Coordinates=coordinates,Cansellation=free_cancellation,Breakfast=breakfast,Prepayment=No_prepayment_needed,Stars=true_stars,Number_of_reviews=true_num_reviews,Review=kind_words,Rating=rating_true,Thumbs_up=thumbs_up_true)
  return(data.frame(output))
}




###HERE GOES SCRAPING
session=html_session("https://www.booking.com/searchresults.html?label=gen173nr-1DCAEoggI46AdIM1gEaFKIAQGYATG4ARfIAQzYAQPoAQH4AQKIAgGoAgO4Atzjm_YFwAIB&sid=3501e222cd28d5cae07e2e64ce96c956&sb=1&src=searchresults&src_elem=sb&error_url=https%3A%2F%2Fwww.booking.com%2Fsearchresults.html%3Flabel%3Dgen173nr-1DCAEoggI46AdIM1gEaFKIAQGYATG4ARfIAQzYAQPoAQH4AQKIAgGoAgO4Atzjm_YFwAIB%3Bsid%3D3501e222cd28d5cae07e2e64ce96c956%3Btmpl%3Dsearchresults%3Bcheckin_month%3D8%3Bcheckin_monthday%3D24%3Bcheckin_year%3D2020%3Bcheckout_month%3D8%3Bcheckout_monthday%3D31%3Bcheckout_year%3D2020%3Bcity%3D900047975%3Bclass_interval%3D1%3Bdest_id%3D900047975%3Bdest_type%3Dcity%3Bdtdisc%3D0%3Bfrom_sf%3D1%3Bgroup_adults%3D1%3Bgroup_children%3D0%3Binac%3D0%3Bindex_postcard%3D0%3Blabel_click%3Dundef%3Bno_rooms%3D1%3Boffset%3D0%3Bpostcard%3D0%3Broom1%3DA%3Bsb_price_type%3Dtotal%3Bshw_aparth%3D1%3Bslp_r_match%3D0%3Bsrc%3Dsearchresults%3Bsrc_elem%3Dsb%3Bsrpvid%3D846896e959990084%3Bss%3DTbilisi%3Bss_all%3D0%3Bssb%3Dempty%3Bsshis%3D0%3Bssne%3DTbilisi%3Bssne_untouched%3DTbilisi%3Btop_ufis%3D1%26%3B&ss=Tbilisi&is_ski_area=0&ssne=Tbilisi&ssne_untouched=Tbilisi&city=900047975&checkin_year=2020&checkin_month=8&checkin_monthday=24&checkout_year=2020&checkout_month=8&checkout_monthday=31&group_adults=2&group_children=0&no_rooms=1&from_sf=1")
response_booking=read_html(session)
data=data.frame(booking_com_function(response_booking),)
df=rbind(df,booking_com_function(response_booking))
df=data.frame()

set.seed(1)

session=html_session("https://www.booking.com/searchresults.html?aid=304142&label=gen173nr-1DCAEoggI46AdIM1gEaFKIAQGYATG4ARfIAQzYAQPoAQH4AQKIAgGoAgO4Atzjm_YFwAIB&sid=3501e222cd28d5cae07e2e64ce96c956&tmpl=searchresults&checkin_month=8&checkin_monthday=24&checkin_year=2020&checkout_month=8&checkout_monthday=31&checkout_year=2020&city=900047975&class_interval=1&dest_id=900047975&dest_type=city&from_sf=1&group_adults=2&group_children=0&label_click=undef&no_rooms=1&raw_dest_type=city&room1=A%2CA&sb_price_type=total&shw_aparth=1&slp_r_match=0&src=searchresults&src_elem=sb&srpvid=3de237571c590077&ss=Tbilisi&ssb=empty&ssne=Tbilisi&ssne_untouched=Tbilisi&top_ufis=1&rows=25&offset=1")
data=NULL
for (i in 1:39){
  response_booking=read_html(session)
  url_test=read_html(gsub(".*Host: *(.*?) *<NA>.*", "\\1", session$handle)[2])
  hotel=unlist(html_nodes(response_booking,css='span.sr-hotel__name')%>%html_text(trim=T))
  price=unlist(html_nodes(url_test,css="div.bui-price-display__value.prco-text-nowrap-helper.prco-inline-block-maker-helper")%>%html_text(trim=T))
  url_to_property=unlist(paste0('https://www.booking.com/',str_remove(html_nodes(response_booking,css="h3>a")%>%html_attr('href'),'\n/')))
  distance_from_center_true=unlist(str_remove(gsub("^.*?map","",str_remove_all(html_nodes(response_booking,css="div.sr_card_address_line")%>%html_text(trim = T),'\n')),'Metro access'))
  coordinates=unlist(html_nodes(response_booking,css="div.sr_card_address_line>a")%>%html_attr('data-coords'))
  free_cancellation=unlist(str_replace(str_replace(str_detect(html_nodes(url_test,css = 'div.roomPrice.roomPrice_flex.sr_discount'), 'FREE cancellation'), pattern = 'TRUE', replacement = 'FREE cancellation'),pattern = 'FALSE', replacement = 'No'))
  breakfast=unlist(str_replace(str_replace(str_detect(html_nodes(url_test,css = 'div.roomPrice.roomPrice_flex.sr_discount'), 'Breakfast included'), pattern = 'TRUE', replacement = 'included'),pattern = 'FALSE', replacement = 'Not inlcuded'))
  No_prepayment_needed=unlist(str_replace(str_replace(str_detect(html_nodes(url_test,css = 'div.roomPrice.roomPrice_flex.sr_discount'), 'No prepayment needed'), pattern = 'TRUE', replacement = 'No prepayment needed'),pattern = 'FALSE', replacement = 'Prepayment needed'))
  true_stars=unlist(str_extract(html_nodes(url_test,css = 'div.sr-hotel__title-wrap>span'), '[0-9]-star hotel'))
  true_num_reviews=unlist(str_extract(gsub(".*score__text *(.*?) *</div>.*", "\\1", html_nodes(response_booking, css = 'div.reviewFloater')), '[0-9]+ reviews'))
  kind_words=unlist(gsub(".*[0-9] *(.*?) *[0-9]+ reviews.*", "\\1",html_nodes(response_booking, css = 'div.reviewFloater')%>%html_text(trim = T)))
  rating_true=unlist(str_remove(gsub( " .*$", "", html_nodes(response_booking, css = 'div.reviewFloater')%>%html_text(trim = T) ), 'Just'))
  thumbs_up_true=unlist(str_replace(str_replace(str_detect(html_nodes(response_booking,css = 'div.sr-hotel__title-wrap>span'),'thumbs_up'),pattern = "TRUE",replacement = "Yes"),pattern = "FALSE",replacement = "No"))
  output=list(Hotel=hotel, hyperlink=url_to_property,Price=price ,Free_Cancellation=free_cancellation,Breakfast=breakfast,Prepayment=No_prepayment_needed, Distance_from_centre=distance_from_center_true, Coordinates=coordinates,Stars=true_stars,Number_of_reviews=true_num_reviews,Review=kind_words,Rating=rating_true,Thumbs_up=thumbs_up_true)
  data=data.frame(output)%>%rbind(data)
  session=follow_link(session,css='li.bui-pagination__item.bui-pagination__next-arrow>a')
  
}

price=unlist(html_nodes(url_test,css="div.bui-price-display__value.prco-text-nowrap-helper.prco-inline-block-maker-helper")%>%html_text(trim=T))
typeof(session$url)
url_booking
as.vector(price)
response_booking=read_html(session)
response_booking
resp_test=read_html(session$url)
html_form(session)


session$handle
url_test=read_html(gsub(".*Host: *(.*?) *<NA>.*", "\\1", session$handle)[2])
resp_test=read_html(url_test)
url_test


true_stars=unlist(str_extract(html_nodes(response_booking,css = 'div.sr-hotel__title-wrap>span.sr-hotel__title-badges'), '[0-9]-star hotel'))
html_nodes(response_booking,css = 'div.sr-hotel__title-wrap>span.sr-hotel__title-badges')[2]

true_stars
true_stars=unlist(str_extract(html_nodes(resp_test,css = 'div.sr-hotel__title-wrap>span'), '[0-9]-star hotel'))
true_stars


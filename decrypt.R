library(lubridate)
library(digest)
key = "abfc8f9dcf8c3f3d8aa294ac5f2cf2cc7767e5592590f39c3f503271dd68562b"
val = "/open/noauth/company-info/ads?api_key=51job&timestamp=1706263198&adsType=2759"
i <- seq(as.integer(ymd_hms("2024-01-26 10:15:00")), as.integer(ymd_hms("2024-01-26 10:25:59")))
glue_i <- glue::glue('/open/noauth/company-info/ads?api_key=51job&timestamp={i}&adsType=2759')
as_datetime(1706264756)
now()
as_datetime(1706266906)
hash <- sprintf('/open/noauth/company-info/ads?api_key=51job&timestamp=%i&adsType=3159', 
                as.integer(now('UTC') + seconds((0:60)))) %>% 
  sapply(function(i) hmac(key = key, object = i, algo = "sha256"))
"f0ac14722840991bc92be49e954be3e8c814e7bee4a493890f6722aef7217b4b" %in% hash
val <- c("/open/noauth/company-info/ads?api_key=51job&timestamp=1706267366&adsType=2759", 
         "/open/noauth/company-info/ads?api_key=51job&timestamp=1706267370&adsType=3197", 
         "/open/noauth/company-info/ads?api_key=51job&timestamp=1706267382&adsType=0455", 
         "/open/noauth/company-info/ads?api_key=51job&timestamp=1706267387&adsType=3159", 
         "/open/noauth/dictionary/batch-dictionary?api_key=51job&timestamp=1706267392&dictionaryNames=d_search_providesalary", 
         "/open/noauth/company-info/pc-info?api_key=51job&timestamp=1706267398&encryCompanyId=AmBVMVI3AD8PbgVgXT0", 
         '/open/noauth/jobs/company?api_key=51job&timestamp=1706267404{"pageNum":1,"pageSize":20,"coId":"4061465","jobArea":"","salaryType":"","function":""}')
sapply(val, function(i) hmac(key = key, object = i, algo = "sha256")
)
as_datetime(c(1706267366, 1706267370, 1706267382, 1706267387, 1706267392, 1706267398, 1706267404))
as_datetime(1706274811)

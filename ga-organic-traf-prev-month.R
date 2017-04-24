# Подключение требуемых пакетов
library(googleAuthR)
library(googleAnalyticsR)
library(googlesheets)

#####
# Достаем данные из справочника проектов.
# Авторизация в Google Spreadsheets и получение данных оттуда
# Создаем токен и сохраняем в файл 
# мануал тут https://rawgit.com/jennybc/googlesheets/master/vignettes/managing-auth-tokens.html
# 
# token <- gs_auth(cache = FALSE)
# gd_token()
# saveRDS(token, file = "spreadsheets.token.rds")

# Загружаем токен из файла
gs_auth(token = "spreadsheets.token.rds")

#Получаем справочник проектов из Google таблицы, заполненой IM  в ручную
spreadsheet <- gs_key("КЛЮЧ-ТАБЛИЦЫ", 
                      lookup = NULL, 
                      visibility = NULL, 
                      verbose = TRUE)


# Читаем из вкладки "projects_list" список проектов и представлений GA, к которым они привязаны.
projects <- gs_read(spreadsheet, ws = 'projects', range = NULL, literal = TRUE)

#####
# Перебор аккаунтов и выгрузка всех представлений
# Имена файлов для хранения токенов = названиям аккаунтов GA
seodep_accounts <- c("seo1.netpeak","seo2.netpeak","seo3.netpeak, seo4.netpeak")
# data frame для представлений GA
allviews <- data.frame()


for(account in seodep_accounts){
  #####
  # Авторизация в Google Analytics. Для хранения токена аккаунта используется отдельный файл с таким же названием как и аккаунт.
  
  # Начало получения токена
  # Заапускаем 2 команды ниже только один раз и потом закомментируем.
  # Задаем область доступа
  # options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics", "https://www.googleapis.com/auth/analytics.readonly"), googleAuthR.httr_oauth_cache = account)
  # 
  # ga_token <- googleAuthR::gar_auth(new_user = TRUE)
  # Конец получения токена
  
  # Подгружаем токен из файла
  auth_token <- readRDS(account)[[1]]
  options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics", 
                                          "https://www.googleapis.com/auth/analytics.readonly"))
  
  googleAuthR::gar_auth(auth_token)
  
  #####
  # Вытягиваем доступные аккаунты и представления чтобы проверять куда расшарены нужные представления
  account_list <- google_analytics_account_list()
  
  # Дописываем название аккаунта
  account_list$seo_dep_acc <- account
  
  # Выделяем список представлений и их аккаунтов
  views <- account_list[, which(names(account_list) %in% c("viewId", "seo_dep_acc"))]
  # Формируем полный список представлений
  allviews <- rbind(allviews, views)
  
  # Удаляем ненужные переменные
  if(exists("views")){rm(views)}
}

#####
# СБОР ДАННЫХ по всем проектам в цикле
# data frame для сохранения 
result_data <- data.frame()

# data frame для ошибок
errors <- data.frame(project = character(0), 
                     obj = character(0), 
                     err = character(0), 
                     stringsAsFactors = F)

# data frame для проектов, которые не расшарены 
not_shared_projects <- data.frame()

# Определяем первое и последнее число предыдущего месяца
som <- function(x) {
  as.Date(format(x, "%Y-%m-01"))
}

start_period <- som(som(Sys.Date()) - 1)
end_period <- som(Sys.Date()) - 1

for(i in 1:nrow(projects)){
  # проверяем находится ли id из справочника в списке id аккаунта
  if(projects$ga_main_view_id[i] %in% allviews$viewId){
    #####
    # Авторизация в Google Analytics. Берем аккаунт, который соответствует id профиля проекта.
    auth_token <- readRDS(allviews$seo_dep_acc[projects$ga_main_view_id[i] == allviews$viewId][1])[[1]]
    options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics", 
                                            "https://www.googleapis.com/auth/analytics.readonly"))
    googleAuthR::gar_auth(auth_token)
    
    
      #####
      #Получение данных Google Analytics
      #ID представления Google Analytics
      ga_view <- paste0("ga:", projects$ga_main_view_id[i]) 
      
      #Запрос к GA API
      er_text <- try(gaData <- google_analytics(id = ga_view, 
                                                start = start_period,
                                                end = end_period, 
                                                metrics = "ga:sessions", 
                                                dimensions = "ga:medium", 
                                                filters = "ga:medium==organic",
                                                max_results = 10000))
      if(!exists("gaData")){
        print(paste0(Sys.time(), " - ", projects$domain[i], " - Get GA data - INVALID -", er_text[1]))
        errors <- data.frame(project = projects$domain[i], obj = "Error in GA" , err = er_text[1])
        next
      }
      
      #####
      # Проверяем есть ли данные из GA
      if(nrow(gaData) > 0){
        # Формируем итоговый dataframe
        totalData <- data.frame(domain = projects$domain[i],
                                ga_sessions = gaData$sessions)
      }
      else{
        totalData <- data.frame(domain = projects$domain[i],
                                ga_sessions = 0)
      }
      
      # Добавляем данные в результирующий дата фрейм
      result_data <- rbind(result_data, totalData)
    }
    else{not_shared_projects <- rbind(not_shared_projects, 
                                      data.frame(domain = projects$domain[i],
                                                 ga_view = projects$ga_main_view_id[i]))}
    
    #####
    # Удаляем ненужные переменные
    if(exists("gaData")){rm(gaData)}
    if(exists("totalData")){rm(totalData)}
  
  
}

#####
# Добавляем информацию про год и месяц
result_data$year_month <- format.Date(Sys.Date(), "%Y%m")
result_data$year_month <- as.integer(result_data$year_month)

#####
# Запись данных в Google Таблицу на лист ga_db. Дописываем построчно в конец.
for(i in 1:nrow(result_data)){
  gs_add_row(spreadsheet, ws="ga_db", input=c(result_data$domain[i],
                                              result_data$ga_sessions[i],
                                              result_data$year_month[i]))
}

# Записываем список нерасшаренных проектов на лист "Нерасшаренные"
gs_edit_cells(ss = spreadsheet,
              ws = "Нерасшаренные",
              input = not_shared_projects,
              anchor = "A1",
              trim = T,
              verbose = T)

#####
# Сохранение результата в csv
write.csv(x = result_data, file = paste0(format.Date(Sys.Date(), "%Y-%m"),
                                         "_GA_organic_traf.csv"))
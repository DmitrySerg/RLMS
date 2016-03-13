
""" 
Моделирование и прогнозирование заболеваемости населения в зависимости от различных социально-экономических факторов
Выявление социально-экономических детерминант здоровья индивида. 

Сергеев Д.А.

"""

library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")
library("rlms")
library("devtools")
library("plyr")
library("stats")
library("pander")
library("gmodels")
library("rms")
library("popbio")
library("coefplot")
library("pscl")


hh_2013 <- read.rlms("r22h_os25a.sav")
hh_frame_2013 <- data.frame(hh_2013)

"""

Загрузка данных RLMS 


"""
ind_2013 <- read.rlms("C:/Users/Auditore/Desktop/representative/22/r22i_os25a.sav")   # 22-я волна 2013 год
saveRDS(ind_2013, "C:/Users/Auditore/Desktop/representative/22/r22i_os25a.Rds")       # Преобразуем в более быстрочитаемый формат для R
ind_2013_frame <- data.frame(ind_2013)



#пол респондента
sex_2013 <- ind_2013_frame[, "rh5"]
sex_2013 <- revalue(sex_2013, c("МУЖСКОЙ"=0, "ЖЕНСКИЙ"=1))
sex_2013 <- as.numeric(as.character(sex_2013))
#год рождения
byear_2013 <- ind_2013_frame[, "rh6"]
# возраст респондента
age_2013 <- 2013 - byear_2013
# возраст в квадрате
agesq_2013 <- age_2013^2
# вес респондента
weight_2013 <- ind_2013_frame[, "rm1"]
hist(weight_2013, main = "Распределение веса", xlab = "Вес индивида (кг)", ylab = "Частота")
# рост респондента
height_2013 <- ind_2013_frame[, "rm2"]
hist(height_2013, main = "Распределение роста", xlab = "Рост индивида (см)", ylab = "Частота")
plot(height_2013, weight_2013, xlab = "Рост индивида (см)", ylab = "Вес индивида (кг)", main = "Распределение роста и веса (2013 г)")
# Индекс массы тела
BMI_2013 <- weight_2013/(height_2013/100)^2

#уровень удовлетворенности жизнью
life_satisfaction_2013 <- ind_2013_frame[, "rj65"]
life_satisfaction_2013 <- revalue(life_satisfaction_2013, c("Полностью удовлетворены" = 1, "Скорее удовлетворены" = 2, 
                                                            "И да, и нет" = 3, "Не очень удовлетворены" = 4, "Совсем не удовлетворены" = 5, "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ"=NA, "ОТКАЗ ОТ ОТВЕТА"=NA, "НЕТ ОТВЕТА"=NA))
life_satisfaction_2013 <- as.numeric(as.character(life_satisfaction_2013))

#где проживает
living_site_type_2013 <- ind_2013_frame[, "status"]
living_site_type_2013 <- dummy.code(living_site_type_2013)
#living_site_type_2013 <- living_site_type_2013[,-2] # исключаем город, чтобы не было мультиколлинеарности
#семейное положение
mar_status_2013 <- ind_2013_frame[, "r_marst"]
#выберем женатых
levels(mar_status_2013)[levels(mar_status_2013)=="Состоите в зарегистрированном браке"]=1
levels(mar_status_2013)[levels(mar_status_2013)!=1]=0
mar_status_2013 <- as.numeric(as.character(mar_status_2013))
mar_status_2013 <- recode(mar_status_2013, "NA=0")
#сколько детей
kids_count_2013 <-ind_2013_frame[, "rj72.172"]
kids_count_2013 <- recode(kids_count_2013, "NA=0")
#образование
educ_2013 <- ind_2013_frame[, "r_educ"]
education_2013 <- revalue(educ_2013, c("0 классов школы"="Неок. среднее", "1 класс школы" = "Неок. среднее", 
                                       "2 класса школы"="Неок. среднее", "3 класса школы" = "Неок. среднее",
                                       "4 класса школы" = "Неок. среднее", "5 классов школы" = "Неок. среднее",
                                       "6 классов школы" = "Неок. среднее", "7 классов школы" = "Неок. среднее",
                                       "8 классов школы" = "Неок. среднее", "9 классов школы" = "Неок. среднее", 
                                       "7-9 классов школы (незак. средн) + ПТУ без диплома" = "Неок. среднее",
                                       "7-9 классов школы (незак. средн) + ПТУ с дипломом" = "Неок. среднее",
                                       "10 и более классов школы без аттестата о среднем образовании" = "Неок. среднее",
                                       "7-9 классов школы (незак. среднее) и менее 2 лет в техникуме" = "Неок. среднее",
                                       "среднее образование - есть аттестат о ср. образовании" = "Среднее",
                                       "10 и более классов школы и какое-либо професс. обр. без диплома" = "Среднее",
                                       "10 и более классов школы и какое-либо професс. обр. с дипломом" = "Среднее тех.",
                                       "10 и более классов школы и техникум без диплома" = "Среднее",
                                       "техникум с дипломом" = "Среднее тех.", 
                                       "1-2 года в высшем учебном заведении" = "Неок. высшее",
                                       "3 и более лет в высшем учебном заведении" = "Неок. высшее",
                                       "есть диплом о высшем образовании" = "Высшее",
                                       "аспирантура и т.п. без диплома" = "Высшее",
                                       "аспирантура и т.п. с дипломом" = "Высшее",
                                       "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ" = NA, "ОТКАЗ ОТ ОТВЕТА" = NA, "НЕТ ОТВЕТА" = NA))

""" 
ЗДОРОВЬЕ

"""


# Как индивид оценивает состояние своего здоровья
health_state_2013 <- ind_2013_frame[, "rm3"]
health_state_2013 <- revalue(health_state_2013, c("Очень хорошее" = 1, "Хорошее" = 2, "Среднее, не хорошее, но и не плохое" = 3, "Плохое" = 4, "Совсем плохое" = 5, "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ" = NA, "ОТКАЗ ОТ ОТВЕТА" = NA, "НЕТ ОТВЕТА" = NA))
health_state_2013 <- as.numeric(as.character(health_state_2013))
# Есть ли у вас заболевания сердца?
heart_2013 <- ind_2013_frame[, "rm20.61"] 
heart_2013 <- revalue(heart_2013, c("Да" = 1, "Нет" = 0))
heart_2013 <- as.numeric(as.character(heart_2013))
heart_2013 <- recode(heart_2013, "NA=0")
# Есть ли у вас заболевания легких, бронхов?
lungs_2013 <- ind_2013_frame[, "rm20.62"]
lungs_2013 <- revalue(lungs_2013, c("Да" = 1, "Нет" = 0))
lungs_2013 <- as.numeric(as.character(lungs_2013))
lungs_2013 <- recode(lungs_2013, "NA=0")
# Есть ли у вас заболевания печени?
liver_2013 <- ind_2013_frame[, "rm20.63"]
liver_2013 <- revalue(liver_2013, c("Да" = 1, "Нет" = 0))
liver_2013 <- as.numeric(as.character(liver_2013))
liver_2013 <- recode(liver_2013, "NA=0")
# Есть ли у вас заболевания почек?
kidneys_2013 <- ind_2013_frame[, "rm20.64"]
kidneys_2013 <- revalue(kidneys_2013, c("Да" = 1, "Нет" = 0))
kidneys_2013 <- as.numeric(as.character(kidneys_2013))
kidneys_2013 <- recode(kidneys_2013, "NA=0")
# Есть ли у вас заболевания желудочно-кишечного тракта?
stomach_2013 <- ind_2013_frame[, "rm20.65"]
stomach_2013 <- revalue(stomach_2013, c("Да" = 1, "Нет" = 0))
stomach_2013 <- as.numeric(as.character(stomach_2013))
stomach_2013 <- recode(stomach_2013, "NA=0")
# Есть ли у вас заболевания позвоночника?
spine_2013 <- ind_2013_frame[, "rm20.66"]
spine_2013 <- revalue(spine_2013, c("Да" = 1, "Нет" = 0))
spine_2013 <- as.numeric(as.character(spine_2013))
spine_2013 <- recode(spine_2013, "NA=0")
# Есть ли у вас заболевания эндокринной системы, диабет или повышенный сахар крови?
diabet_2013 <- ind_2013_frame[, "rm20.68"]
diabet_2013 <- revalue(diabet_2013, c("Да" = 1, "Нет" = 0))
diabet_2013 <- as.numeric(as.character(diabet_2013))
diabet_2013 <- recode(diabet_2013, "NA=0")
# Есть ли у вас гипертоническая болезнь, повышенное артериальное давление?
bl_pressure_2013 <- ind_2013_frame[, "rm20.69"]
bl_pressure_2013 <- revalue(bl_pressure_2013, c("Да" = 1, "Нет" = 0))
bl_pressure_2013 <- as.numeric(as.character(bl_pressure_2013))
bl_pressure_2013 <- recode(bl_pressure_2013, "NA=0")
# Есть ли у вас заболевания суставов?
joints_2013 <- ind_2013_frame[, "rm20.610"]
joints_2013 <- revalue(joints_2013, c("Да" = 1, "Нет" = 0))
joints_2013 <- as.numeric(as.character(joints_2013))
joints_2013 <- recode(joints_2013, "NA=0")
# Есть ли у вас заболевание ЛОР-органов?
otolaring_2013 <- ind_2013_frame[, "rm20.611"]
otolaring_2013 <- revalue(otolaring_2013, c("Да" = 1, "Нет" = 0))
otolaring_2013 <- as.numeric(as.character(otolaring_2013))
otolaring_2013 <- recode(otolaring_2013, "NA=0")
# Есть ли у вас неврологические заболевания?
neuro_2013 <- ind_2013_frame[, "rm20.612"]
neuro_2013 <- revalue(neuro_2013, c("Да" = 1, "Нет" = 0))
neuro_2013 <- as.numeric(as.character(neuro_2013))
neuro_2013 <- recode(neuro_2013, "NA=0")
# Есть ли у вас заболевания глаз?
eyes_2013 <- ind_2013_frame[, "rm20.613"]
eyes_2013 <- revalue(eyes_2013, c("Да" = 1, "Нет" = 0))
eyes_2013 <- as.numeric(as.character(eyes_2013))
eyes_2013 <- recode(eyes_2013, "NA=0")
# Есть ли у вас аллергия?
allergy_2013 <- ind_2013_frame[, "rm20.615"]
allergy_2013 <- revalue(allergy_2013, c("Да" = 1, "Нет" = 0))
allergy_2013 <- as.numeric(as.character(allergy_2013))
allergy_2013 <- recode(allergy_2013, "NA=0")
# Есть ли у вас варикозное расширение вен?
veins_2013 <- ind_2013_frame[, "rm20.616"]
veins_2013 <- revalue(veins_2013, c("Да" = 1, "Нет" = 0))
veins_2013 <- as.numeric(as.character(veins_2013))
veins_2013 <- recode(veins_2013, "NA=0")
# Есть ли у вас заболевания кожного покрова?
skin_2013 <- ind_2013_frame[, "rm20.617"]
skin_2013 <- revalue(skin_2013, c("Да" = 1, "Нет" = 0))
skin_2013 <- as.numeric(as.character(skin_2013))
skin_2013 <- recode(skin_2013, "NA=0")
# Есть ли у вас онкологические заболевания?
cancer_2013 <- ind_2013_frame[, "rm20.618"]
cancer_2013 <- revalue(cancer_2013, c("Да" = 1, "Нет" = 0))
cancer_2013 <- as.numeric(as.character(cancer_2013))
cancer_2013 <- recode(cancer_2013, "NA=0")
# Есть ли у вас гинекологические заболевания?
gynec_2013 <- ind_2013_frame[, "rm20.614"]
gynec_2013 <- revalue(gynec_2013, c("Да" = 1, "Нет" = 0))
gynec_2013 <- as.numeric(as.character(gynec_2013))
gynec_2013 <- recode(gynec_2013, "NA=0")
# Есть ли у вас заболевания мочеполовой системы?
genit_2013 <- ind_2013_frame[, "rm20.619"]
genit_2013 <- revalue(genit_2013, c("Да" = 1, "Нет" = 0))
genit_2013 <- as.numeric(as.character(genit_2013))
genit_2013 <- recode(genit_2013, "NA=0")
# Назначена ли вам какая-нибудь группа по инвалидности?
disable_2013 <- ind_2013_frame[, "rm20.7"]
disable_2013 <- revalue(disable_2013, c("Да" = 1, "Нет" = 0))
disable_2013 <- as.numeric(as.character(disable_2013))
disable_2013 <- recode(disable_2013, "NA=0")

#была ли беременность
preg_2013 <- ind_2013_frame[, "rn4"]
preg_2013 <- revalue(preg_2013, c("Да"= 1, "Нет" = 0))
preg_2013 <- as.numeric(as.character(preg_2013))
preg_2013 <- recode(preg_2013, "NA=0")

# Удается ли питаться регулярно, не реже 3х раз в день
food_2013 <- ind_2013_frame[, "rm152"]
food_2013 <- revalue(food_2013, c("Да, удаётся" = 1, "Скорее да, чем нет"=1, "Скорее нет, чем да"=0, "Никогда не удаётся"=0))
food_2013 <- as.numeric(as.character(food_2013))
food_2013 <- recode(food_2013, "NA=0")
# За последние 30 дней принимали какие-либо витамины, минеральные вещества, БАДы
vitamins_2013 <- ind_2013_frame[, "rm117.0"]
vitamins_2013 <- revalue(vitamins_2013, c("Да"= 1, "Нет" = 0))
vitamins_2013 <- as.numeric(as.character(vitamins_2013))
vitamins_2013 <- recode(vitamins_2013, "NA=0")
# За последние 12 месяцев сидели на какой-нибудь диете?
diet_2013 <- ind_2013_frame[, "rm117.1"]
diet_2013 <- revalue(diet_2013, c("Да"= 1, "Нет" = 0))
diet_2013 <- as.numeric(as.character(diet_2013))
diet_2013 <- recode(diet_2013, "NA=0")



"""
РАБОТА

"""


#профессия
#occupation_2013 <- ind_2013_frame[, "r_occup"]
#трудовой стаж
#working_experience_2013 <- ind_2013_frame[, "rj73.3y"]
#сколько работал после выхода на пенсию
work_after_pension_2013 <- ind_2013_frame[, "rj73.4m"]
work_after_pension_2013 <- recode(work_after_pension_2013, "NA=0")
#уровень з/п
wage_2013 <- ind_2013_frame[, "rj10"]

#уровень удовлетворенности работой
job_satisfaction_2013 <- ind_2013_frame[, "rj1.1.1"]
job_satisfaction_2013 <- revalue(job_satisfaction_2013, c("ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ" = 1, "СКОРЕЕ УДОВЛЕТВОРЕНЫ" = 2, 
                                                          "И ДА, И НЕТ" = 3, "СКОРЕЕ НЕ УДОВЛЕТВОРЕНЫ" = 4, "СОВСЕМ НЕ УДОВЛЕТВОРЕНЫ" = 5, "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ"=NA, "ОТКАЗ ОТ ОТВЕТА"=NA, "НЕТ ОТВЕТА"=NA))
job_satisfaction_2013 <-as.numeric(as.character(job_satisfaction_2013))
#уровень удовлетворенности з/п
wage_satisfaction_2013 <- ind_2013_frame[, "rj1.1.3"]
wage_satisfaction_2013 <- revalue(wage_satisfaction_2013, c("ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ"=1, "СКОРЕЕ УДОВЛЕТВОРЕНЫ" = 2, 
                                                            "И ДА, И НЕТ"=3,"СКОРЕЕ НЕ УДОВЛЕТВОРЕНЫ"=4, "СОВСЕМ НЕ УДОВЛЕТВОРЕНЫ"=5, "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ"=NA, "ОТКАЗ ОТ ОТВЕТА"=NA, "НЕТ ОТВЕТА"=NA))
wage_satisfaction_2013 <- as.numeric(as.character(wage_satisfaction_2013))
#уровень удовлетворенности условиями труда
wrking_cond_satisfaction_2013 <- ind_2013_frame[, "rj1.1.2"]
wrking_cond_satisfaction_2013 <- revalue(wrking_cond_satisfaction_2013, c("ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ"=1, "СКОРЕЕ УДОВЛЕТВОРЕНЫ" = 2, 
                                                                          "И ДА, И НЕТ"=3,"СКОРЕЕ НЕ УДОВЛЕТВОРЕНЫ"=4, "СОВСЕМ НЕ УДОВЛЕТВОРЕНЫ"=5, "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ"=NA, "ОТКАЗ ОТ ОТВЕТА"=NA, "НЕТ ОТВЕТА"=NA))
wrking_cond_satisfaction_2013 <- as.numeric(as.character(wrking_cond_satisfaction_2013))
#длительность рабочей недели (часы)
work_week_duration_2013 <- ind_2013_frame[, "rj6.2"]
#был ли индивид в отпуске последние 12 месяцев
vacation_2013 <- ind_2013_frame[, "rj21a"]
vacation_2013 <- revalue(vacation_2013, c("Да" = 1, "Нет" = 0))
vacation_2013 <- as.numeric(as.character(vacation_2013))
vacation_2013 <- recode(vacation_2013, "NA=0")
#насколько беспокоит возможность потерять работу
job_loss_concern_2013 <- ind_2013_frame[, "rj31"]
job_loss_concern_2013 <- revalue(job_loss_concern_2013, c("Очень беспокоит" = 1, "Немного беспокоит" = 2, 
                                                          "И да, и нет" = 3, "Не очень беспокоит" = 4, "Совсем не беспокоит" = 5))
job_loss_concern_2013 <- as.numeric(as.character(job_loss_concern_2013))
#насколько индивид удовлетворен материальным положением
money_condition_satisfaction_2013 <- ind_2013_frame[, "rj66.1"]
money_condition_satisfaction_2013 <- revalue(money_condition_satisfaction_2013, c("Полностью удовлетворены" = 1, "Скорее удовлетворены" = 2, 
                                                                                  "И да, и нет" = 3, "Не очень удовлетворены" = 4, "Совсем не удовлетворены" = 5))
money_condition_satisfaction_2013 <- as.numeric(as.character(money_condition_satisfaction_2013))
#есть ли работа у индивида
has_job_2013 <- ind_2013_frame[, "rj77"]
has_job_2013 <- revalue(has_job_2013, c("РЕСПОНДЕНТ СЕЙЧАС РАБОТАЕТ, НАХ-СЯ В ОПЛАЧИВ. ИЛИ НЕОПЛАЧ.ОТПУСКЕ, В Т.Ч. ДЕКРЕТНОМ ИЛИ ПО УХОДУ ЗА РЕБЕНКОМ ДО 3 ЛЕТ"=1, "У РЕСПОНДЕНТА НЕТ РАБОТЫ"=0))
has_job_2013 <- as.numeric(as.character(has_job_2013))
#есть ли у индивида еще одна работа
has_2_jobs_2013 <- ind_2013_frame[, "rj32"]
has_2_jobs_2013 <- revalue(has_2_jobs_2013, c("Да"=1, "Нет"=0))
has_2_jobs_2013 <- as.numeric(as.character(has_2_jobs_2013))
has_2_jobs_2013 <- recode(has_2_jobs_2013, "NA=0")
#хотел бы индивид сменить работу
wants_new_job_2013 <- ind_2013_frame[, "rj81"]
wants_new_job_2013 <- revalue(wants_new_job_2013, c("Да" = 1, "Нет" = 0))
wants_new_job_2013 <- as.numeric(as.character(wants_new_job_2013))



"""
КУРЕНИЕ

"""


#курит ли индивид
smokers_2013 <- ind_2013_frame[, "rm71"]
smokers_2013 <- revalue(smokers_2013, c("Да" = 1, "Нет"= 0))
smokers_2013 <- as.numeric(as.character(smokers_2013))
smokers_2013 <- recode(smokers_2013, "NA=0")
#в каком возрасте начал курить
smokers_age_2013 <- ind_2013_frame[,"rm72"]
hist(smokers_age_2013, breaks = 20, main = "В каком возрасте респондент начал курить", xlab="Возраст респондента", ylab = "Частота", freq = TRUE)
#курил ли в последние 7 дней
smoked_7days_2013 <- ind_2013_frame[,"rm73"]
smoked_7days_2013 <- revalue(smoked_7days_2013, c("Да"=1, "Нет"=0))
smoked_7days_2013 <- as.numeric(as.character(smoked_7days_2013))
#количесвто выкуриваемых сигарет
cigarettes_2013 <- ind_2013_frame[, "rm75"]

#цена пачки сигарет
cig_price_2013 <- ind_2013_frame[, "rm149"]
#будет ли курить, если цену в 2 раза поднимут
cig_increase_2013 <- ind_2013_frame[, "rm150"]

#курил ли когда-нибудь (бредовая переменная)
smoked_ever_2013 <- ind_2013_frame[, "rm77"]
smoked_ever_2013 <- revalue(smoked_ever_2013, c("Да"=1, "Нет"=0))
smoked_ever_2013 <- as.numeric(as.character(smoked_ever_2013))
smoked_ever_2013 <- recode(smoked_ever_2013, "NA=0")
#сколько лет назад бросил
smoked_canceled_2013 <- ind_2013_frame[, "rm78"]



"""
АЛКОГОЛЬ

"""


#пьет ли индивид (иногда)
alko_2013 <- ind_2013_frame[,"rm80.0"]
alko_2013 <- revalue(alko_2013, c("Да, употребляете"= 1, "Нет, никогда не употребляете"=0))
alko_2013 <- as.numeric(as.character(alko_2013))
alko_2013 <- recode(alko_2013, "NA=0")
#люди, употреблявшие алкоголь за последние 30 дней
alko_2013_recent <- ind_2013_frame[, "rm80"]
alko_2013_recent <- revalue(alko_2013_recent, c("Да"=1, "Нет"= 0 ))
alko_2013_recent <- as.numeric(as.character(alko_2013_recent))
#как часто пьет (дамми или уровни?)
alko_frequency_2013 <- ind_2013_frame[, "rm81"]
#пьете ли в барах/ресторанах?
alko_2013_bar <- ind_2013_frame[, "rm83.2"]
alko_2013_bar <- revalue(alko_2013_bar, c("Да"=1, "Нет"=0))
alko_2013_bar <- as.numeric(as.character(alko_2013_bar))
#пьете ли дома?
alko_2013_home <- ind_2013_frame[, "rm83.1"]
alko_2013_home <- revalue(alko_2013_home, c("Да"=1, "Нет"=0))
alko_2013_home <- as.numeric(as.character(alko_2013_home))
#пьете ли на работе/учебе?
alko_2013_work <- ind_2013_frame[, "rm83.4"]
alko_2013_work <- revalue(alko_2013_work, c("Да"=1, "Нет"=0))
alko_2013_work <- as.numeric(as.character(alko_2013_work))
#пьете ли в гостях?
alko_2013_guest <- ind_2013_frame[, "rm83.5"]
alko_2013_guest <- revalue(alko_2013_guest, c("Да"=1, "Нет"=0))
alko_2013_guest <- as.numeric(as.character(alko_2013_guest))
#сколько дней в месяц пили промышленное пиво
beer_days_2013 <- ind_2013_frame[, "rm84111d"]
#сколько грамм пива в день
beer_amount_2013 <- ind_2013_frame[, "rm84111b"]
beer_amount_2013 <- recode(beer_amount_2013, "NA=0")
#пил ли пиво за последние 30 дней
beer_recent_2013 <- ind_2013_frame[, "rm84111a"]
beer_recent_2013 <- revalue(beer_recent_2013, c("Да"=1, "Нет"=0))
beer_recent_2013 <- as.numeric(as.character(beer_recent_2013))
beer_recent_2013 <- recode(beer_recent_2013, "NA=0")
#пил ли вино/шампанское последние 30 дней
wine_recent_2013 <- ind_2013_frame[, "rm84.21a"]
wine_recent_2013 <- revalue(wine_recent_2013, c("Да"=1, "Нет"=0))
wine_recent_2013 <- as.numeric(as.character(wine_recent_2013))
wine_recent_2013 <- recode(wine_recent_2013, "NA=0")
#сколько дней в месяц пили вино/шампанское
wine_days_2013 <-ind_2013_frame[, "rm84.21d"]
#сколько грамм вина/шампанского в день
wine_amount_2013 <- ind_2013_frame[, "rm84.21b"]
wine_amount_2013 <- recode(wine_amount_2013, "NA=0")
#пил ли мартини/вермут последние 30 дней
martini_recent_2013 <- ind_2013_frame[, "rm84.31a"]
martini_recent_2013 <- revalue(martini_recent_2013, c("Да"=1, "Нет"=0))
martini_recent_2013 <- as.numeric(as.character(martini_recent_2013))
martini_recent_2013 <- recode(martini_recent_2013t, "NA=0")
#сколько грамм мартини/вермута
martini_amount_2013 <- ind_2013_frame[, "rm84.31b"]
martini_amount_2013 <- recode(martini_amount_2013, "NA=0")
#сколько дней в месяц мартини/вермут
martini_days_2013 <- ind_2013_frame[, "rm84.31d"]
#люди, пившие водку за последние 30 дней
vodka_recent <- ind_2013_frame[, "rm84.5a"]
vodka_recent <- revalue(vodka_recent, c("Да"=1, "Нет"=0))
vodka_recent <- as.numeric(as.character(vodka_recent))
vodka_recent <- recode(vodka_recent, "NA=0")
#сколько грамм водки за последние 30 дней
vodka_amount_2013 <- ind_2013_frame[, "rm84.5b"]
vodka_amount_2013 <- recode(vodka_amount_2013, "NA=0")
#сколько дней в месяц пил водку
vodka_days_2013 <- ind_2013_frame[, "rm84.5d"]
#пил ли коньяк в последние 30 дней
whiskey_recent_2013 <- ind_2013_frame[, "rm84.9a"]
whiskey_recent_2013 <- revalue(whiskey_recent_2013, c("Да"=1, "Нет"=0))
whiskey_recent_2013 <- as.numeric(as.character(whiskey_recent_2013))
whiskey_recent_2013 <- recode(whiskey_recent_2013, "NA=0")
#сколько грамм коньяка/виски/ликера в день
whiskey_amount_2013 <- ind_2013_frame[, "rm84.9b"]
whiskey_amount_2013 <- recode(whiskey_amount_2013, "NA=0")
#сколько дней пил виски
whiskey_days_2013 <- ind_2013_frame[, "rm84.9d"]
#пил ли коктейли
cocktail_recent_2013 <- ind_2013_frame[, "rm84.7a"]
cocktail_recent_2013 <- revalue(cocktail_recent_2013, c("Да"=1, "Нет"=0))
cocktail_recent_2013 <- as.numeric(as.character(cocktail_recent_2013))
#сколько дней в месяц
cocktail_days_2013 <- ind_2013_frame[, "rm84.7d"]
#количество этанола в день (градус умножить на объем)
ethanol_pc <- data.frame(beer_amount_2013, martini_amount_2013,vodka_amount_2013, whiskey_amount_2013, wine_amount_2013)
ethanol_pc <- mutate(ethanol_pc, beer_amount_2013=beer_days_2013*beer_amount_2013*0.06, 
                     martini_amount_2013 = martini_days_2013*martini_amount_2013*0.15, 
                     vodka_amount_2013 = vodka_days_2013*vodka_amount_2013*0.4, 
                     whiskey_amount_2013 = whiskey_days_2013*whiskey_amount_2013*0.45, 
                     wine_amount_2013 =wine_days_2013*wine_amount_2013*0.15)
ethanol_pc <- mutate(ethanol_pc, ethanol_total = as.numeric(rowSums(ethanol_pc[,1:5],na.rm = TRUE)))
ethanol_total <- ethanol_pc[,6]
# Экспертные алкоголики
alko_200 <- as.numeric(ethanol_total>=200)


"""
СПОРТ

"""

#Вы занимались в течение последних 12 месяцев по меньшей мере 12 раз бегом трусцой, катанием на коньках, лыжах?
run_2013 <- ind_2013_frame[, "rm113.1a"]
run_2013 <- revalue(run_2013, c("Да"=1, "Нет"=0))
run_2013 <- as.numeric(as.character(run_2013))
run_2013 <- recode(run_2013, "NA=0")
#Вы занимались в течение последних 12 месяцев по меньшей мере 12 раз упражнениями на тренажерах?
gym_2013 <- ind_2013_frame[, "rm11311a"]
gym_2013 <- revalue(gym_2013, c("Да"=1, "Нет"=0))
gym_2013 <- as.numeric(as.character(gym_2013))
gym_2013 <- recode(gym_2013, "NA=0")
#Вы занимались в течение последних 12 месяцев по меньшей мере 12 раз прогулочной ходьбой?
walk_2013 <- ind_2013_frame[, "rm11312a"]
walk_2013 <- revalue(walk_2013, c("Да"=1, "Нет"=0))
walk_2013 <- as.numeric(as.character(walk_2013))
walk_2013 <- recode(walk_2013, "NA=0")
#Вы занимались в течение последних 12 месяцев по меньшей мере 12 раз ездой на велосипеде?
bycicle_2013 <- ind_2013_frame[, "rm11314a"]
bycicle_2013 <- revalue(bycicle_2013, c("Да"=1, "Нет"=0))
bycicle_2013 <- as.numeric(as.character(bycicle_2013))
bycicle_2013 <- recode(bycicle_2013, "NA=0")
#Вы занимались в течение последних 12 месяцев по меньшей мере 12 раз плаванием?
swim_2013 <- ind_2013_frame[, "rm113.2a"]
swim_2013 <- revalue(swim_2013, c("Да"=1, "Нет"=0))
swim_2013 <- as.numeric(as.character(swim_2013))
swim_2013 <- recode(swim_2013, "NA=0")
#Вы занимались в течение последних 12 месяцев по меньшей мере 12 раз танцами, аэробикой, шейпингом, йогой?
aerobic_2013 <- ind_2013_frame[, "rm113.3a"]
aerobic_2013 <- revalue(aerobic_2013, c("Да"=1, "Нет"=0))
aerobic_2013 <- as.numeric(as.character(aerobic_2013))
aerobic_2013 <- recode(aerobic_2013, "NA=0")
#Вы занимались в течение последних 12 месяцев по меньшей мере 12 раз баскетболом, волейболом, футболом, хоккеем?
football_2013 <- ind_2013_frame[, "rm113.4a"]
football_2013 <- revalue(football_2013, c("Да"=1, "Нет"=0))
football_2013 <- as.numeric(as.character(football_2013))
football_2013 <- recode(football_2013, "NA=0")
#Вы занимались в течение последних 12 месяцев по меньшей мере 12 раз борьбой?
box_2013 <- ind_2013_frame[, "rm11351a"]
box_2013 <- revalue(box_2013, c("Да"=1, "Нет"=0))
box_2013 <- as.numeric(as.character(box_2013))
box_2013 <- recode(box_2013, "NA=0")


"""
ОПИСАТЕЛЬНАЯ СТАТИСТИКА

"""

# Health state
health <- revalue(ind_2013_frame[, "rm3"], c("ЗАТРУДНЯЮСЬ ОТВЕТИТЬ" = NA, "ОТКАЗ ОТ ОТВЕТА" = NA, "НЕТ ОТВЕТА" = NA))
health <- health[!is.na(health)]
qplot(health, main = "Самоценка состояния здоровья индивида", xlab="", ylab="")

table(health_state_2013, sex_2013, exclude=NULL)



men_smokers <- na.omit(data.frame(sex_2013, smokers_2013))
men_smokers <- filter(men_smokers, men_smokers[,1]==1)
men_smokers$smokers_2013 <- as.character(men_smokers$smokers_2013)
men_smokers$smokers_2013[men_smokers$smokers_2013==1] <- "Да"
men_smokers$smokers_2013[men_smokers$smokers_2013==0] <- "Нет"
qplot(men_smokers)

hist(smoked_canceled_2013, main ="Сколько лет назад репондент бросил курить", xlab = "Количество лет", ylab = "Частота")
hist(age_2013, main="Возрастное распределение", xlab="", ylab="Количество наблюдений")
h <- as.numeric(educ_2013)
hist(h, main="Распределение уровня образования", ylab = "Количество наблюдений", xlab = "Код уровня образования")

q <- qplot(life_satisfaction_2013, main = "Степерь удовлетворенности жизнью", xlab="", ylab = "Количество наблюдений")
q + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(panel.background = element_rect(fill = 'white', colour = 'white'))

q <- qplot(educ_2013,main = "Распределение образования", xlab="", ylab = "Количество наблюдений")
q + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(panel.background = element_rect(fill = 'white', colour = 'white'))

q <- qplot(health_state_2013,main = "Самооценка состояния здоровья", xlab="", ylab = "Количество наблюдений")
q + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(panel.background = element_rect(fill = 'white', colour = 'white'))

q <- qplot(data$BMI_2013[-max(data$BMI_2013)], main="Индекс массы тела", xlab = "BMI", ylab="Количество наблюдений")
q + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(panel.background = element_rect(fill = 'white', colour = 'white'))

hist(working_experience_2013, main = "Тудовой стаж", xlab = "Количество лет", ylab = "Количество наблюдений")
hist(work_week_duration_2013, main = "Продолжительность рабочей недели", xlab = "Часы", ylab = "Количество наблюдений")
qplot(working_experience_2013,life_satisfaction_2013,geom="boxplot")

hist(smokers_age_2013, main="В каком возрасте начал курить", xlab="", ylab="Количество наблюдений")
hist(cigarettes_2013, main = "Количество выкуриваемых в день сигарет", xlab="", ylab="Количество наблюдений")

legend("topleft",text.width =1.8, fill = c("red", "blue"), legend=c("Истинные значения","Модельные значения"), horiz=FALSE)

par(mfrow=c(2, 2))
hist(beer_amount_2013[beer_amount_2013>0], main = "Количество выпиваемого в день пива (граммы)", 
     xlab=paste("Среднее значение", round(mean(beer_amount_2013),2), "грамм"), ylab="Количество наблюдений")
hist(wine_amount_2013[wine_amount_2013>0], main = "Количество выпиваемого в день вина (граммы)", 
     xlab=paste("Среднее значение", round(mean(wine_amount_2013),2), "грамм"), ylab="Количество наблюдений")
hist(vodka_amount_2013[vodka_amount_2013>0], main = "Количество выпиваемой в день водки (граммы)", 
     xlab=paste("Среднее значение", round(mean(vodka_amount_2013),2), "грамм"), ylab="Количество наблюдений")
hist(whiskey_amount_2013[whiskey_amount_2013>0], main = "Количество выпиваемого в день коньяка (граммы)", 
     xlab=paste("Среднее значение", round(mean(whiskey_amount_2013),2), "грамм"), ylab="Количество наблюдений")
par(mfrow=c(1, 1))
hist(ethanol_total[ethanol_total<2000 & ethanol_total >20], breaks=30, main="Распределение потребленного этанола в месяц", xlab = "Граммы", ylab = "Количество наблюдений")

for(i in 1:(ncol(health_data_2013)-2)) {
  print(colnames(health_data_2013)[i])
  print(colnames(health_data_2013)[i+1])
  print(CrossTable(health_data_2013[,i], health_data_2013[,i+1]))
  print("X-squared")
  print(chisq.test(table(health_data_2013[,i], health_data_2013[,i+1]))$statistic)
  print("P-value")
  
  print(chisq.test(table(health_data_2013[,i], health_data_2013[,i+1]))$p.value)
}


data <- cbind(health_state_2013, heart_2013, lungs_2013, liver_2013, kidneys_2013, stomach_2013, spine_2013, diabet_2013, 
      bl_pressure_2013, joints_2013, otolaring_2013, neuro_2013, eyes_2013, allergy_2013, 
      veins_2013, skin_2013, cancer_2013, gynec_2013, genit_2013, disable_2013, BMI_2013, preg_2013, food_2013, vitamins_2013,
      diet_2013, age_2013, agesq_2013, sex_2013, life_satisfaction_2013, living_site_type_2013, mar_status_2013, kids_count_2013, education_2013,
      work_after_pension_2013, vacation_2013, money_condition_satisfaction_2013, 
      has_job_2013, has_2_jobs_2013, smokers_2013, 
      smoked_ever_2013, alko_2013, beer_recent_2013, wine_recent_2013, vodka_recent, whiskey_recent_2013, ethanol_total, alko_200,
      run_2013, gym_2013, walk_2013, bycicle_2013, swim_2013, aerobic_2013, football_2013, box_2013)


data <- as.data.frame(data)

for(i in 1:length(names(data))){
  print(names(data)[i])
  print(sum(is.na(data[,i])))
}

data <- na.omit(data)
write.csv(data, "C:/Users/Auditore/Desktop/Диплом/data.csv", row.names=FALSE)

describe(data)

obl_center <- data[,30]
city <- data$город
rural <- data$село
PGT <- data$ПГТ


data_vis <- select(data, heart_2013, lungs_2013, liver_2013, kidneys_2013, stomach_2013, spine_2013, diabet_2013, 
                   bl_pressure_2013, joints_2013, otolaring_2013, neuro_2013, eyes_2013, allergy_2013, 
                   veins_2013, skin_2013, cancer_2013, gynec_2013, genit_2013, disable_2013,preg_2013, food_2013, vitamins_2013,
                   diet_2013, mar_status_2013, kids_count_2013,has_job_2013, has_2_jobs_2013, smokers_2013,smoked_ever_2013, 
                   alko_2013, beer_recent_2013, wine_recent_2013, vodka_recent, whiskey_recent_2013, alko_200,
                   run_2013, gym_2013, walk_2013, bycicle_2013, swim_2013, aerobic_2013, football_2013, box_2013)

data_ill <- select(data, heart_2013, lungs_2013, liver_2013, kidneys_2013, stomach_2013, spine_2013, diabet_2013, 
                   bl_pressure_2013, joints_2013, otolaring_2013, neuro_2013, eyes_2013, allergy_2013, 
                   veins_2013, skin_2013, cancer_2013, gynec_2013, genit_2013, disable_2013)

data_sport <- select(data, run_2013, gym_2013, walk_2013, bycicle_2013, swim_2013, aerobic_2013, football_2013, box_2013)

for(i in 1:length(names(data_ill))){
  print(paste(names(data_ill)[i], table(data_ill[,i])[2], ((table(data_ill[,i])[2])/12622)*100))
}
"""

Табличка Тихомирова


"""


health <- dummy.code(health_state_2013)
colnames(health) <- c("Очень хорошее", "Хорошее", "Среднее, не хорошее, но и не плохое", "Плохое", "Совсем плохое")

sex <- dummy.code(sex_2013)
colnames(sex) <- c("Мужской", "Женский")

life_sat <- dummy.code(life_satisfaction_2013)
colnames(life_sat) <- c("Полностью удовлетворены жизнью", "Скорее удовлетворены жизнью", "И да, и нет", "Не очень удовлетворены жизнью", "Совсем не удовлетворены жизнью")

marriage <- dummy.code(mar_status_2013)
colnames(marriage) <- c("Не состою в браке","Состою в браке")

job_sat <- dummy.code(job_satisfaction_2013)
colnames(job_sat) <- c("Полностью удовлетворены работой", "Скорее удовлетворены работой", "И да, и нет", "Не очень удовлетворены работой", "Совсем не удовлетворены работой")

money_sat <- dummy.code(money_condition_satisfaction_2013)
colnames(money_sat) <- c("Полностью удовлетворены материальным положением", "Скорее удовлетворены материальным положением", "И да, и нет", "Не очень удовлетворены материальным положением", "Совсем не удовлетворены материальным положением")

illness <- cbind(health, heart_2013, lungs_2013, liver_2013, kidneys_2013, stomach_2013, spine_2013, diabet_2013, 
                 bl_pressure_2013, joints_2013, otolaring_2013, neuro_2013, eyes_2013, allergy_2013, 
                 veins_2013, skin_2013, cancer_2013, gynec_2013, genit_2013, disable_2013)
illness <- as.data.frame(illness)

social_econ <- cbind(sex, life_sat, living_site_type_2013, marriage, preg_2013, food_2013, vitamins_2013, dummy.code(education_2013), 
                     job_sat, vacation_2013, money_sat, has_job_2013, has_2_jobs_2013, smokers_2013, 
                     smoked_ever_2013, alko_2013, beer_recent_2013, wine_recent_2013, vodka_recent, whiskey_recent_2013, run_2013, gym_2013, 
                     walk_2013, bycicle_2013, swim_2013, aerobic_2013, football_2013, box_2013)
social_econ <- as.data.frame(social_econ)


freq_table <- matrix(0, nrow=length(names(social_econ)), ncol=length(names(illness)))

for(ill in 1:length(names(illness))){
  for(econ in 1:length(names(social_econ))){
    freq_table[econ, ill] <- sum(illness[,ill][social_econ[,econ]==1], na.rm=TRUE)
  }
}

frequency_table <- freq_table/16087

rownames(freq_table) <- names(social_econ)
colnames(freq_table) <- names(illness)
rownames(frequency_table) <- names(social_econ)
colnames(frequency_table) <- names(illness)


# Multiple histograms
par(mfrow=c(4, 6))
colnames <- dimnames(data_vis)[[2]]
for (i in 1:24) {
  plot(as.factor(data_vis[,i]), main=colnames[i], col="gray", border="white")
}
for (i in 25:49) {
  plot(as.factor(data_vis[,i]), main=colnames[i], col="gray", border="white")
}

qplot(as.factor(health_state_2013), fill=as.factor(sex_2013), facets=~sex_2013, data=data, xlab = "Оценка состояния здоровья", ylab="Количество наблюдений")
qplot(as.factor(mar_status_2013), fill=as.factor(sex_2013), facets=~sex_2013, data=data)

"""   
Cross Table кторые мне нужны

"""

CrossTable(data$health_state, data$sex_2013, chisq=TRUE)

# Оценка здоровья и ЗАБОЛЕВАНИЯ
CrossTable(data$health_state_2013, data$heart_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$lungs_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$liver_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$kidneys_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$stomach_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$spine_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$diabet_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$bl_pressure_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$joints_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$otolaring_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$neuro_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$eyes_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$allergy_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$veins_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$skin_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$cancer_2013, chisq=TRUE)
CrossTable(data$health_state_2013[data$sex_2013==1], data$gynec_2013[data$sex_2013==1], chisq=TRUE)
CrossTable(data$health_state_2013, data$genit_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$disable_2013, chisq=TRUE)

CrossTable(data$health_state_2013, data$diet_2013, chisq=TRUE) # сидевшие на диете хуже оценивают здоровье
CrossTable(data$health_state_2013[data$sex_2013==1], data$preg_2013[data$sex_2013==1], chisq=TRUE)

# Оценка здоровья и СПОРТ
CrossTable(data$health_state_2013, data$run_2013, chisq=T)
CrossTable(data$health_state_2013, data$gym_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$walk_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$bycicle_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$swim_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$aerobic_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$football_2013, chisq=TRUE)
CrossTable(data$health_state_2013, data$box_2013, chisq=TRUE)

chisq.test(table(data$health_state_2013, data$run_2013))
chisq.test(table(data$health_state_2013, data$gym_2013))
chisq.test(table(data$health_state_2013, data$walk_2013))
chisq.test(table(data$health_state_2013, data$bycicle_2013))
chisq.test(table(data$health_state_2013, data$swim_2013))
chisq.test(table(data$health_state_2013, data$aerobic_2013))
chisq.test(table(data$health_state_2013, data$football_2013))
chisq.test(table(data$health_state_2013, data$box_2013))


# Оценка здоровья и РАБОТА
CrossTable(data$health_state_2013, data$has_job_2013, chisq=T)


assocplot(table(data$health_state_2013, data$life_satisfaction_2013), #слева - уровень удовлетворенности, снизу - здоровье
          col = c("light green", "red"), main="Связь между оценкой здоровья и уровнем удовлетворенности жизнью")

# Заболевания и ПОЛ
CrossTable(data$heart_2013, data$sex_2013, chisq=TRUE)
CrossTable(data$lungs_2013, data$sex_2013, chisq=TRUE)
CrossTable(data$disable_2013, data$sex_2013, chisq=TRUE)
CrossTable(data$stomach_2013, data$sex_2013, chisq=TRUE)
CrossTable(data$cancer_2013, data$sex_2013, chisq=TRUE)


# Логит-модельки
lrm(data=data, heart_2013~sex_2013+age_2013)
coefplot(glm(data=data, heart_2013~sex_2013+age_2013, family="binomial"))
lrm(data=data, heart_2013~run_2013+gym_2013+walk_2013+bycicle_2013+swim_2013+aerobic_2013+football_2013+box_2013)

logi.hist.plot(data$age_2013,data$heart_2013,boxp=F,type="hist",col="gray", xlab="Возраст респондента", main="Заболевания сердца и возраст")
logi.hist.plot(data$age_2013,data$bl_pressure_2013,boxp=FALSE,type="hist",col="gray", xlab="Возраст респондента", main="Заболевания гипертонии и возраст")
logi.hist.plot(data$BMI_2013,data$diabet_2013,boxp=F,type="hist",col="gray", xlab="Индекс массы тела", main="Заболевания диабетом и ИМТ")
logi.hist.plot(data$BMI_2013,data$joints_2013,boxp=F,type="hist",col="gray", xlab="Индекс массы тела", main="Заболевания суставов и ИМТ")

# СЕРДЦЕ


# без статистического взаимодействия
model <- glm(data=data, heart_2013~age_2013+sex_2013, family="binomial")
lrm(data=data, heart_2013~age_2013+sex_2013)
pR2(model)
model_predict1 <- predict(model, data, se=TRUE)
model_tab <- cbind(data, model_predict1)
model_tab <- mutate(model_tab, prob=plogis(fit))
qplot(data=model_tab, x=model_tab$age_2013, y=fit, geom="point", xlab="Возраст", ylab="Лог-шанс") + theme(panel.background = element_rect(fill = "#F8F8F8", colour = 'grey'))
qplot(data=model_tab, x=model_tab$age_2013, y=prob, geom="point", xlab="Возраст", ylab="Вероятность") + theme(panel.background = element_rect(fill = "#F8F8F8", colour = 'grey'))



# со статистическим взаимодействием
model <- glm(data=data, heart_2013~age_2013+I(age_2013 * sex_2013), family="binomial")
summary(model)

lrm(data=data, heart_2013~age_2013+age_2013 * sex_2013)

model_predict <- predict(model, data, se=TRUE)
model_tab <- cbind(data, model_predict)
model_tab <- mutate(model_tab, prob=plogis(fit))
qplot(data=model_tab, x=model_tab$age_2013, y=fit, geom="point", xlab="Возраст", ylab="Лог-шанс")+ theme(panel.background = element_rect(fill = "#F8F8F8", colour = 'grey'))
qplot(data=model_tab, x=model_tab$age_2013, y=prob, geom="point", xlab="Возраст", ylab="Вероятность") + theme(panel.background = element_rect(fill = "#F8F8F8", colour = 'grey'))



# Большая модель для сердца
model <- glm(data=data, heart_2013~age_2013+bl_pressure_2013+BMI_2013+mar_status_2013+alko_2013, family="binomial")
summary(model)
pR2(model)
q <- coefplot(model, main="", xlab="Значение", ylab="",
              newNames=c(age_2013="Возраст",
                         bl_pressure_2013="Повышенное артериальное давление",
                         BMI_2013="ИМТ",
                         mar_status_2013="Семейное положение",
                         alko_2013="Пьёт ли иногда"))
q + theme(panel.background = element_rect(fill = "#F8F8F8", colour = 'grey'))
lrm(data=data, heart_2013~age_2013+bl_pressure_2013+BMI_2013+mar_status_2013+alko_200)



# НЕВРОЛОГИЯ
dummy <- dummy.code(data$money_condition_satisfaction_2013)
dummy <- dummy[,-3]
model <- glm(data=data, neuro_2013~age_2013+sex_2013+has_job_2013+has_2_jobs_2013+cancer_2013+kids_count_2013+alko_2013+dummy, family="binomial")
pR2(model)
q <- coefplot(model, main="", xlab="Значение", ylab="", 
              newNames=c(dummy5="Совсем не удовлетворен",
                         dummy4="Скорее не удовлетворен",
                         dummy2="Скорее удовлетворен",
                         dummy1="Полностью удовлетворен",
                         alko_2013="Пьёт ли иногда",
                         kids_count_2013="Количество детей",
                         cancer_2013="Наличие онкологического заболевания",
                         has_2_jobs_2013="Наличие второй работы",
                         has_job_2013="Наличие работы",
                         sex_2013="Пол",
                         age_2013="Возраст"))

                                                                            
q + theme(panel.background = element_rect(fill = "#F8F8F8", colour = 'grey'))
summary(model)
lrm(data=data, neuro_2013~age_2013+sex_2013+has_job_2013+has_2_jobs_2013+cancer_2013+kids_count_2013+alko_2013+dummy)
          


# ПЕЧЕНЬ
model <- glm(data=data, liver_2013~age_2013+sex_2013+alko_200+kidneys_2013+smokers_2013+BMI_2013+run_2013+stomach_2013, family="binomial")
pR2(model)
q <- coefplot(model, main="", xlab="Значение", ylab="",
              newNames=c(BMI_2013="ИМТ",
                         smokers_2013="Курит ли индивид",
                         kidneys_2013="Есть ли заболевания почек",
                         alko_200="Употребляет больше 200 грамм этанола",
                         sex_2013="Пол",
                         age_2013="Возраст",
                         run_2013="Бег, коньки, лыжи",
                         stomach_2013="Есть ли заболевания желудка"))
q + theme(panel.background = element_rect(fill = "#F8F8F8", colour = 'grey'))
lrm(data=data, liver_2013~age_2013+sex_2013+alko_200+kidneys_2013+smokers_2013+BMI_2013+run_2013+stomach_2013)
summary(model) 



# СУСТАВЫ
lrm(data=data, joints_2013~age_2013+sex_2013+BMI_2013+vacation_2013)
model <- glm(data=data, joints_2013~age_2013+sex_2013+BMI_2013+vacation_2013, family="binomial")
summary(model)
pR2(model)
q <- coefplot(model, main="", xlab="Значение", ylab="",
              newNames=c(BMI_2013="ИМТ",
                         sex_2013="Пол",
                         age_2013="Возраст",
                         vacation_2013="Был ли в отпуске"))
q + theme(panel.background = element_rect(fill = "#F8F8F8", colour = 'grey'))



# ЛЕГКИЕ
model <- glm(data=data, lungs_2013~age_2013+smokers_2013 + BMI_2013+ село + город + ПГТ, family="binomial")
summary(model)
pR2(model)
q <- coefplot(model, main="", xlab="Значение", ylab="",
              newNames=c(age_2013="Возраст",
                         smokers_2013="Курит ли индивид",
                         BMI_2013="ИМТ",
                         село="Проживает в селе",
                         город="Проживает в городе",
                         ПГТ="Проживает в ПГТ"))
q + theme(panel.background = element_rect(fill = "#F8F8F8", colour = 'grey'))

summary(model)
lrm(data=data, lungs_2013~age_2013+smokers_2013 + BMI_2013+ rural+city+PGT)


# ЖЕЛУДОК
model <- glm(data=data, stomach_2013~age_2013 + sex_2013 + smokers_2013+ diabet_2013+diet_2013+liver_2013+neuro_2013+obl_center+rural+PGT, family="binomial")
lrm(data=data, stomach_2013~age_2013 + sex_2013 + smokers_2013 +diabet_2013+ diet_2013 + liver_2013+neuro_2013+obl_center+rural+PGT)
pR2(model)

q <- coefplot(model, main="", xlab="Значение", ylab="",
              newNames=c(age_2013="Возраст",
                         sex_2013="Пол",
                         smokers_2013="Курит ли индивид",
                         diabet_2013="Наличие диабета",
                         diet_2013="Сидел ли на диете",
                         liver_2013="Наличие болезни печени",
                         neuro_2013="Наличие неврологических заболеваний",
                         rural="Проживает в селе",
                         obl_center="Проживает в областном центре",
                         PGT="Проживает в ПГТ"))
q + theme(panel.background = element_rect(fill = "#F8F8F8", colour = 'grey'))




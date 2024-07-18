##ANALYZING THE MOVIE DATASET 

install.packages("stringr")
library(dplyr)
library(stringr)

#Importing the data set
city <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/city.csv')
company <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/company.csv')
company_film <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/company_film.csv')
company_grant <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/company_grant.csv')
company_shareholder <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/company_shareholder.csv')
country <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/country.csv')
crew <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/crew.csv')
crew_info <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/crew_info.csv')
department <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/department.csv')
department_address <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/department_address.csv')
employee <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/employee.csv')
film <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/film.csv')
grant_request <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/grant_request.csv')
kind_of_organization <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/kind_of_organization.csv')
phone_number <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/phone_number.csv')
registration_body <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/registration_body.csv')
role <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/role.csv')
shareholder <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/shareholder.csv')
staff <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/staff.csv')
staff_salary <- read.csv('C:/Users/LDO SYSTEMS/Desktop/Movie Production Companies in Europe Dataset/staff_salary.csv')

#Viewing the different data
View(company)
View(company_film)
View(company_grant)
View(company_shareholder)
View(country)
View(crew)
View(crew_info)
View(department)
View(department_address)
View(employee)
View(film)
View(grant_request)
View(kind_of_organization)
View(phone_number)
View(registration_body)
View(role)
View(shareholder)
View(staff)
View(staff_salary)

#creating dummy files for analysis
city_df <- city
company_df <- company
company_film_df <- company_film
company_grant_df <- company_grant
company_shareholder_df <- company_shareholder
country_df <- country
crew_df <- crew
crew_info_df <- crew_info
department_df <- department
department_address_df <- department_address
employee_df <- employee
film_df <- film
grant_request_df <- grant_request
kind_of_organization_DF <- kind_of_organization
phone_number_df <- phone_number
registration_body_df <- registration_body
role_df <- role


##Obtaining production companies information
companies_in_europe <- company$name
companies_in_europe <- data.table::data.table(companies_in_europe)
View(companies_in_europe)

#Renaming id columns so as to be used for joining
city_df <- city_df %>%
  rename(city_id = id)
View(city_df)

country_df <- country_df %>%
  rename(country_code = code)
View(country_df)

kind_of_organization_DF <- kind_of_organization_DF %>%
  rename(kind_of_organization_id = id)

#Joining columns by the unique identifier ID
company_city <- left_join(company_df, city_df[, c("city_id","name")], by = "city_id")
company_city <- company_city %>%
  rename(city = name.y)
View(company_city)

company_country <- left_join(company_df, country_df[, c("country_code","name")], by = "country_code")
company_country <- company_country %>%
  rename(nation = name.y)
View(company_country)

company_org <- left_join(company_df, kind_of_organization_DF[, c("kind_of_organization_id","name")], by = "kind_of_organization_id")
company_org <- company_org %>%
  rename(organization_type = name.y)

#Getting the net value
net_value <- company_df$total_asset - company_df$total_liability
net_value <- data.table::data.table(net_value)
View(net_value)

#finding out the number of employees for a company
count <- table(employee_df$company_id)
count_df <- as.data.frame.table(count)
colnames(count_df) <- c("Value", "Count")
View(count_df)

#Binding columns
company_df <- cbind(company_df, city = company_city$city)
company_df <- cbind(company_df, nation = company_country$nation)
company_df <- cbind(company_df, organization_type = company_org$organization_type)
company_df <- cbind(company_df, net_value = net_value$net_value)
company_df <- cbind(company_df, No_of_employees = count_df$Count)
View(company_df)

#Merging and renaming the main selected columns from company_df
companies_in_europe <- data.frame(cbind(company_df$name, company_df$address, company_df$zip_code, company_df$city, company_df$nation, company_df$organization_type, company_df$No_of_employees, company_df$net_value))

companies_in_europe <- companies_in_europe %>%
  rename(nation = X5, organization_type = X6, no_of_employees = X7, net_value = X8)
View(companies_in_europe)


#Obtaining shareholder information similar steps
shareholder <- shareholder %>%
  rename(shareholder_id = id)

company_df <- company_df %>%
  rename(company_id = id)

movie_shareholder <- left_join(shareholder, company_shareholder[, c("shareholder_id","company_id")], by = "shareholder_id")

movie_shareholder_df <- left_join(movie_shareholder, company_df[, c("company_id","name")], by = "company_id")
View(movie_shareholder_df)

company_shareholders <- cbind(movie_shareholder_df$name, movie_shareholder_df$first_name, movie_shareholder_df$last_name, movie_shareholder_df$place_of_birth, movie_shareholder_df$personal_telephone, movie_shareholder_df$national_insurance_number, movie_shareholder_df$passport_number)
View(company_shareholders)

company_shareholders <- data.frame(company_shareholders)

company_shareholders <- company_shareholders %>%
  rename(company_name = X1, first_name = X2, last_name = X3, place_of_birth = X4, personal_telephone = X5, national_insurance_number = X6, passport_number = X7)


#obtaining employees information still using same steps
employees_df <- employees_df %>%
  rename(employee_id = id)

employees_df <- employees_df %>%
  rename(company_name = name)

employees_df <- left_join(employee, company_df[, c("company_id", "name")], by = "company_id")

employees_df <- left_join(employees_df, phone_number[, c("employee_id", "phone")], by = "employee_id")

company_employees <- data.frame(cbind(employees_df$company_name, employees_df$employee_id,employees_df$first_name,employees_df$last_name,employees_df$middle_name,employees_df$gender,employees_df$date_of_birth,employees_df$date_started,employees_df$phone,employees_df$email_address,employees_df$employee_role))

company_employees <- company_employees %>%
  rename(company_name=X1, employee_id=X2, first_name=X3, last_name=X4, middle_name=X5, gender=X6, date_of_birth=X7, date_started=X8, phone=X9, email_address=X10, emloyee_role=11)


##Obtaining compensation information using same steps 

#For crew members
role <- role %>%
  rename(role_id = id) 

crew_members <- left_join(crew, role[, c("role_id","name")], by = "role_id")

crew_salary <- left_join(crew, crew_info[, c("crew_id","hourly_rate", "daily_bonus")], by = "crew_id")

crew_members_info <- data.frame(cbind(crew_members$name, crew_salary$hourly_rate, crew_salary$daily_bonus))

crew_members_info <- crew_members_info %>%
  rename(crew_members = X1, hourly_rate = X2, daily_bonuses = X3)

#For staff members
department <- department %>%
  rename(department_id = id)

staff_members <- left_join(staff, department[, c("department_id", "name")], by = "department_id")

staff_wage <- left_join(staff_members, staff_salary[, c("staff_id", "working_hours", "salary")], by = "staff_id")

staff_members_info <- data.frame(cbind(staff_wage$staff_id, staff_wage$salary, staff_wage$working_hours, staff_wage$name))

staff_members_info <- staff_members_info %>%
  rename(staff_members_id = X1, monthly_wage = X2, working_hours = X3, department_affiliation = X4)


#Obtaining film information
film_company <- film_company %>%
  rename(movie_code = film_movie_code)

film_company <- left_join(company_film, company_df[, c("company_id", "name")], by = "company_id")

film_title <- left_join(film_company, film[, c("movie_code", "title", "release_year")], by = "movie_code")

crew_role <- left_join(crew_info, role[, c("role_id", "name")], by = "role_id")

crew_title <- left_join(crew_role, film_title[, c("movie_code", "name", "title", "release_year")], by = "movie_code", relationship = "many-to-many")

film_information <- data.frame(cbind(crew_title$name.y, crew_title$title, crew_title$movie_code, crew_title$release_year, crew_title$name.x))

film_information <- film_information %>%
  rename(production_company = X1, title_of_film = X2, movie_code = X3, release_year = X4, crew_members = X5)


#Obtaining grant information
grant_request <- grant_request %>%
  rename(grant_id = id)

company_grantname <- left_join(company_grant, company_df[, c("company_id", "name")], by = "company_id")

company_grantRequest <- left_join(grant_request, company_grantname[, c("grant_id", "name")], by = "grant_id")

grant_information <- data.frame(cbind(company_grantRequest$name, company_grantRequest$title, company_grantRequest$funding_organization, company_grantRequest$maximum_monetary_value, company_grantRequest$deadline, company_grantRequest$application_date, company_grantRequest$desired_amount, company_grantRequest$status))

grant_information <- grant_information %>%
  rename(productin_company = X1, grant_title = X2, funding_organization = X3, maximum_monetary_value = X4, proposal_deadline = X5, application_date = X6, desired_amount = X7, outcome = X8)


##List of all the important variables

#1. Retrieved production companies information = companies_in_europe
#2. Retrieved shareholders information = company_shareholders
#3. Retrieved employees information = company_employees
#4. Retrieved compensation information for crew members = crew_members_info
#5. Retrieved compensation information for staff members = staff_members_info
#6. Retrieved film information = film_information
#7. Retrieved grant information = grant_information 

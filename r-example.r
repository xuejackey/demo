
#Malika Mahoui 
# pdf/word to txt using R library
# this code extract document sections up to 3 levels and their contents
# 5/3/2019
# Layer ARN: arn:aws:lambda:us-east-2:131329294410:layer:r-runtime-3_6_0:12
library(stringr)
library(pdftools) #install.packages("pdftools") #version 1.8
library(textreadr)
library(data.table)
library(readr)
library(jsonlite)



args <- commandArgs(trailingOnly = TRUE)

fileInput<- args[1]
current_wd <- args[2]


list_not_desired_keywords_inFileName = character() #list of keywords to be used to fitler filenames

##Functions
read_non_desired_keywords <- function() # when TOC does not exist we output whehter the fileName include on of the keywords that are indicative of non relevnat file like approval
{
  list_not_desired_keywords_inFileName = character()
  tryCatch({
    x <- scan(paste0(current_wd,"/","not_desired_keywords_inFileName.txt"), what = list(wordstoFilter_list = character()))
    list_not_desired_keywords_inFileName <- x$wordstoFilter_list
  }, error = function(e) {
    print("not able to read the file with the words to use to filter the files")
  })
  if (length(list_not_desired_keywords_inFileName) ==0)
  {
    # assign the default values
    list_not_desired_keywords_inFileName <- c("(moo)","add","adde","addend","addm","adend","amend","amendment summary","appendices","appendix","approval","brochure","calculator","certific","change","change_list","checklist","checklist_complete","cn","communication","copy","copyof","copyright","diagram","discrepancy","discussion","do not use","docapproval","docapprv","documentation","draft","erb","errata","es","esp","eu pas","extension","filipino","healthcare","initial","italy","j","japan","japanese","justification","kor","korea","korean","manual of operation","notification","protocolo","qr","questionnaire","rationale","register","regulatory response","review","signature","sinopsis","sp","spanish","supplement","tables","track change","tracking","translation","worksheet")
  }
  return(list_not_desired_keywords_inFileName)
}

check_fileName_include_nonDesired_keywords <- function(fileName)
{
  for (word_inList in list_not_desired_keywords_inFileName)
  {
    if (length(grep(paste0("(\\b|_)",word_inList,"(\\b|_)"), fileName, ignore.case=TRUE))!=0)
     return(word_inList)
  }
  return("")
}

is_non_english_txt <- function(txt)
{
    if (length(grep("[\u3040-\u30ff]", txt))>0)
    {
      files_not_processed <<- rbind(files_not_processed, list(fileName,"pdf_with_non_english_characters"))
      return(TRUE)
    }
  length(txt)
  return(FALSE)
}
is_empty_txt <- function(txt)
{
    if (length(txt)==0)
    {
      files_not_processed <<- rbind(files_not_processed, list(fileName,"pdf_to_text_is_empty"))
      print("pdf_to_text_is_empty")
      return(TRUE)
    } 
    if (length(grep("[:alnum:]+",txt))==0)
      {
      files_not_processed <<- rbind(files_not_processed, list(fileName,"pdf_to_text_has_empty_pages"))
      print("pdf_to_text_has_empty_pages")
      return(TRUE)
    }
  
  return(FALSE)
}

get_section_number_info <- function(current_title)
{
  #return section number, the current level of the section and the text after section number
  section_number_title = sub("^\\s*([^[:blank:]]+)(\\s+.+)$","\\1;;\\2",current_title)
  section_number_title_arr = unlist(strsplit(section_number_title,";;"))
  section_number = section_number_title_arr[1]
  section_number = sub("^(.+)(\\.0)$","\\1",section_number) # e.g. replace 1.0 by 1 to determine the section level
  after_section_number =section_number_title_arr[2]
  
  section_levels = unlist(strsplit(section_number,"\\."))
  return(list(section_number, length(section_levels),after_section_number))
}

log_error <- function(error_message,fileName, nb_pages_in_document)
{
  non_desired_keyword_inFileName = check_fileName_include_nonDesired_keywords(fileName)
  if (non_desired_keyword_inFileName!="")
  { 
    files_not_processed <<- rbind(files_not_processed, list(fileName,paste0(error_message ," and file name includes ", non_desired_keyword_inFileName," keyword")))
    print(paste0(error_message ," and file name includes ", non_desired_keyword_inFileName," keyword"))
  } else 
  {
    if (length(grep("imclone",txt, ignore.case = TRUE))!=0)
    {
      files_not_processed <<- rbind(files_not_processed, list(fileName,paste0(error_message ," and file sponsored by ImClone")))
      print(paste0(error_message ," and file sponsored by ImClone"))
    } else 
    {
      if (nb_pages_in_document <5 )
        {nb_pages="<5"
      } else 
        {
          if (nb_pages_in_document <=10)
          {nb_pages="between 5 and 10"
          } else 
          {nb_pages=">10"}
        }
        files_not_processed <<- rbind(files_not_processed, list(fileName,paste0(error_message , " and nb_pages_in_document ",nb_pages)))
      print(paste0(error_message , " and nb_pages_in_document= ",nb_pages_in_document))
    }
  }
  return()
}

detect_header_footer <- function(page_start_index, txt,header_footer)
{
  if ((page_start_index+2)>length(txt))
  {
    return(0)
  }
  more_header_footer =TRUE
  nb_header_footer_lines = 0
  first_page = unlist(strsplit(txt[page_start_index],"\n"))
  if(header_footer=="header") {i=1
  }else {i=length(first_page) }
  index_direction =0
  while (more_header_footer && index_direction<length(first_page))
  {
    first_page = unlist(strsplit(txt[page_start_index],"\n"))
    if(header_footer=="header") {first_page_line = first_page[i+index_direction]
    }else {first_page_line = first_page[i-index_direction]}
    
    current_header_footer = sub("(^.+Page).+$","\\1",first_page_line,ignore.case = T) # acocunt for different ways to ahve page  number (page 1 or 20) (page 1)
    if (current_header_footer==first_page_line)
    { current_header_footer = sub("(^.+(\\.|\\s|\\-))\\s*(\\d+|[ivx]+)(\\s*\\-)?\\s*$","\\1",first_page_line)} # case where page number is not preceded with Page
   
     current_header_footer =trimws(gsub("\\s+"," ",current_header_footer))
    for (j in 1:2)
    {
      next_page = unlist(strsplit(txt[page_start_index+j],"\n"))
      if(header_footer=="header") {next_page_line = next_page[1+index_direction]
      }else {next_page_line = next_page[length(next_page)-index_direction]}
    
      next_header_footer = sub("(^.+Page).+$","\\1",next_page_line,ignore.case = T) # acocunt for different ways to ahve page  number (page 1 or 20) (page 1)
      if (next_header_footer==next_page_line)
      { next_header_footer = sub("(^.+(\\.|\\s|\\-))\\s*(\\d+|[ivx]+)(\\s*\\-)?\\s*$","\\1",next_page_line,ignore.case = T)} # case where page number is not preceded with Page
      
      next_header_footer =trimws(gsub("\\s+"," ",next_header_footer))
      if (current_header_footer!=next_header_footer)
      {
        more_header_footer=FALSE
       break
      }
    }
    if (more_header_footer) {nb_header_footer_lines = nb_header_footer_lines+1}
    index_direction=index_direction + 1
  }
  return(nb_header_footer_lines)
}

remove_header_footer_lines <- function(txt_input,nb_header_lines,nb_footer_lines)
{
  clean_txt = character()
  for (i in 1:length(txt_input))
  {
    temp = unlist(strsplit(txt_input[i],"\n"))
    temp =temp[(nb_header_lines+1):(length(temp)-nb_footer_lines)]
    clean_txt = append(clean_txt, paste0(temp, collapse = "\n"))
  }
  return(clean_txt)
}
extract_TableOfContents <- function(fileName,txt)
{
  
  dt_content_table = data.table(level=character(), level_number=character(), title=character(), page=character(),index_in_TOC=character(), title_start = character())
  #locate first page of TableOfContents
  nb_pages_in_document = length(txt) # this is used to detect that page number is witihn the document length; if there were corrections in the page numbers; all number appear
  
  firstPage = integer()
  firstPage <- grep("(^|\n)\\s*\\d?\\.?\\s*TABLE OF CONTENTS\\s*\n",txt, ignore.case = T, perl=T)
  non_desired_keyword_inFileName = check_fileName_include_nonDesired_keywords(fileName)
  if (length(firstPage)==0)
      {
        log_error("table of contents not found",fileName, nb_pages_in_document)
        return(dt_content_table)
      }
  
  #locate last page of TableOfContents
    lastPage <- grep("\\d{0,2}\\.?\\s*References",txt[firstPage:(firstPage+10)], ignore.case = T, perl = TRUE)
    if (length(lastPage)==0)
    {
      lastPage <- grep("(APPENDICES)|(Attachment)|(List of tables)|(list of Figures)|(Table\\s*Page)|(Figure\\s*Page)",txt[firstPage:(firstPage+10)], ignore.case = T, perl=T)
      if (length(lastPage)==0)
        {
          log_error("not able to detect the end of last page in table of content",fileName, nb_pages_in_document)
          return(dt_content_table)
        }
    }
    firstp <-firstPage[1]
    lastp <- firstPage[1]+lastPage[1]-1
    
    nb_header_lines <<- detect_header_footer(firstp, txt,"header")
    nb_footer_lines <<- detect_header_footer(firstp, txt,"footer")
    
    #remove header and footer lines from TOC
    cleaned_TOC = remove_header_footer_lines(txt[firstp:lastp],nb_header_lines,nb_footer_lines)
    tableContentLines <- unlist(strsplit(cleaned_TOC,"\n"))
    
    #find the line where to stop procesisng TOC
    line_stop_in_TOC = grep("(^|\\s+)References",tableContentLines, ignore.case = T, perl=T)
    if (length(line_stop_in_TOC)==0)
    {
      line_stop_in_TOC = grep("(APPENDICES)|(Attachment)|(List of tables)|(list of Figures)|(Table\\s*Page)|(Figure\\s*Page)",tableContentLines, ignore.case = T, perl=T)
    }
    tableContentLines = tableContentLines[1:line_stop_in_TOC[1]]
    
    #an expected systax  found in doc: Clinical Protocol.docx_0901870c800882bb.pdf  
    #14.1.1. 12.2.7. Secondary Efficacy Outcomes and Methodology ........................................72"
    
    #locate line with table of content and skip it
    TOC_loc = grep("Table of Contents",tableContentLines, ignore.case = T, perl=T)
    #locate the line with Section Page - it indicates the start of the titles in the TOC
    section_page_loc = grep("(Section\\s\\s+Page)|(Sections)$",tableContentLines, ignore.case = T, perl=T)
    
    # skip to the part of the TOC page after the TOC landmark or Section Page landmark 
    if(length(section_page_loc)!=0)
      {i = (max(TOC_loc[1],section_page_loc[1]) +1)
    }else {  i = (TOC_loc[1] +1)}
    
    # detect whether the TOC has page numbers of sections
    page_number_exist = grep("(\\.+|\\s+)\\s*(\\d+)\\s*$",tableContentLines[i:(length(tableContentLines) -1 )])
    if (length(page_number_exist)<4) #we need to have at least 3 titles in the current page with page number 
    {
      log_error("no page numbers in the TOC",fileName, nb_pages_in_document)
      return(dt_content_table)
    }
    #this case is used to filter document with special character that appear to be \\s but the are not - the option is to remove these files
    # we assume there must be at least 50% of lines in TOC with page number
    if (length(page_number_exist) < (length(tableContentLines[i:(length(tableContentLines) -1 )])*0.5))
    {
      log_error("no enough page numbers identified in the TOC - often due to non standard space characters",fileName, nb_pages_in_document)
      return(dt_content_table)
    }
        
    while (i<=length(tableContentLines) & length(grep("^((1|2)\\.?0?)([^[:alnum:]\\.])",tableContentLines[i]))==0) # skip until first title
    {i = i+1}
    
    if (i>length(tableContentLines)) 
       {
      log_error("title in table of contents does not start from 1",fileName, nb_pages_in_document)
      return(dt_content_table)
    } #test if we have titles after locating the first section title number that should start with 1
    

    tableContentLines = tableContentLines[i:length(tableContentLines)]


    TOC_titles_loc = grep("(^\\s*)(\\d{1,2}(\\.\\d+){0,4}\\.?)\\s+",tableContentLines) # assuming that a title part does not start with a number of at most 2 digits
    
    #remove header and footer lines
    tableContentLines_copy = character()
    for (k in 1:length(tableContentLines))
    {
     if ((k %in% TOC_titles_loc) || (length(grep("^\\s+",tableContentLines[k]))!=0 && (length(grep("Page\\s+\\d+",tableContentLines[k],ignore.case = T))==0)))
        tableContentLines_copy = append(tableContentLines_copy,tableContentLines[k])
    }
    tableContentLines = tableContentLines_copy
    #need to locate the starting of the title in the cleaned table of content
    #used this epxression becuase some of the docuemtns have special characters that look like space but are not idnetified as such by regular expression
    TOC_titles_loc = grep("(^\\s*)(\\d{1,2}(\\.\\d+){0,4}\\.?)\\s+",tableContentLines) # assuming that a title part does not start with a number of at most 2 digits
    
    #remove false title number - e.g line starting with 26 weeks
    TOC_titles_loc_copy = integer()
    TOC_titles_loc_copy  = append(TOC_titles_loc_copy, TOC_titles_loc[1])
    for (l in 2:length(TOC_titles_loc))
    {
     previous_line = tableContentLines[TOC_titles_loc[l]-1]
      if (length(grep("\\d+\\s*$",previous_line))!=0) #if the line before this new title does not have a page number the the current line is not a title line
      {
        TOC_titles_loc_copy= append(TOC_titles_loc_copy, TOC_titles_loc[l])
      } 
    }
    TOC_titles_loc= TOC_titles_loc_copy
    previous_page_number = 0 #used to check whether the new page number is greater that the previous page number
    
    for (k in 1:length(TOC_titles_loc))
     {
      section_number_info = unlist(get_section_number_info(tableContentLines[TOC_titles_loc[k]]))
      section_number = section_number_info[1]
      level_section = section_number_info[2]
      after_section_number = section_number_info[3]
      j=TOC_titles_loc[k]

      section_title_array = character()
      while ((k<length(TOC_titles_loc)) && (j<(TOC_titles_loc[k+1]-1)))
      {
        # this line is not the line that include page number
        # extract this part of title text and keep adding to it
        # we assume that if there is no leading space then the text is not part of title; propably header of footer
        # except for the first line of the title that may not stat with leading spaces

        if (length(grep("^\\s+",tableContentLines[j]))!=0 || (j==TOC_titles_loc[k]))
        {
          section_title_array =  append(section_title_array,trimws(tableContentLines[j]))
       }
        j=j+1 
      }

      # process the part of the title with page number and store the title into the dataframe
      title = paste(list(paste(section_title_array, collapse= " "),trimws(tableContentLines[j])), collapse= " ") # merge the first part of title if exist plus rest of title
      title_and_pageNumber= sub("(\\d+(\\.\\d+){0,4}\\.?)\\s+(.+[^\\.]+)((\\.{1,}|\\s+)\\s*(\\d+|(ERROR.*)))\\s*$","\\3;;\\6", title,ignore.case = TRUE)
      
      if (title_and_pageNumber[1]!=title)
      {
        title_and_pageNumber = unlist(strsplit(title_and_pageNumber,";;"))
        section_title = trimws(title_and_pageNumber[1])
        if (j==TOC_titles_loc[k]) # the title was in one line
        {
          title_start = trimws(title_and_pageNumber[1])
        } else
        {
          #extract the start title is in the first line of the title
          title_start = trimws(after_section_number)
        }
        if (length(grep("ERROR",title_and_pageNumber[2],ignore.case = TRUE))==0 && (as.numeric(trimws(title_and_pageNumber[2]))>= previous_page_number) )
        {page_number = as.numeric(trimws(title_and_pageNumber[2])) 
        }else { #there is the eror message or the page number is not correct typically captured when the current page number is less than the previous one
          page_number = 0  # another procedure will attempt to find a page number
        }
        if (as.numeric(page_number)>nb_pages_in_document) # one situation of redacted pages ; not all cases of redacted numbers will be  caught
        { 
          log_error("pages numbers in table of contents redacted; ignore file",fileName, nb_pages_in_document)
          dt_content_table = data.table(level=character(), level_number=character(), title=character(), page=character(),index_in_TOC=character(), title_start = character())
          return(dt_content_table)
        }
      }
      else # expected title to end with page number or ERROR
      {
        log_error("need to examine; expected title to end with page number or ERROR; ignore file",fileName, nb_pages_in_document)
        dt_content_table = data.table(level=character(), level_number=character(), title=character(), page=character(),index_in_TOC=character(), title_start = character())
        return(dt_content_table)
      }
      
      #store title into  dt_content_table
      section_title = gsub("\\-\\s+", '\\-', section_title)  # when concatennate title, remove space if added after dash as dosh was supposed to be without space after
      index_in_TOC = TOC_titles_loc[k]
      dt_content_table = rbind(dt_content_table,list(level_section,section_number,trimws(section_title),as.numeric(page_number),index_in_TOC,title_start))
      previous_page_number = as.numeric(page_number)
      k= k+1 #move into the second starting of a tilte
      
    } #end processing all titles
    
    dt_content_table = subset(dt_content_table,level<4)
    #dt_content_table = correct_erroneous_pages(dt_content_table, tableContentLines) #not needed after remving header and footer info from TOC
    return(dt_content_table)
}

correct_erroneous_pages <- function(dt_content_table, tableContentLines)
{
  # we assume the page of the first title is correct
  i=2
  while (i<nrow(dt_content_table)) 
  {
    if ((as.numeric(dt_content_table[i]$page) < as.numeric(dt_content_table[i-1]$page))  && (as.numeric(dt_content_table[i]$page)!=0))
    {
      #locate the line in the table of content  that has the real page number for the title
      index_start = as.numeric(dt_content_table[i]$index_in_TOC) + 1
      index_end = as.numeric(dt_content_table[i+1]$index_in_TOC) 
      # the part of the text taken as page number is part of the title
      section_title = paste(c(dt_content_table[i]$title, dt_content_table[i]$page), collapse= " ")
      if (index_start < index_end)
      {
        while (index_start <(index_end-1)) # the last line we missed should have the page number 
        {
          section_title <- paste(c(section_title, trimws(tableContentLines[index_start])), collapse= " ")
          index_start = index_start +1
        }
        #process the line that has the page number
        title_and_pageNumber= sub("^(.+[^\\.]+)((\\.+|\\s+)\\s*(\\d+|(ERROR.*)))$","\\1;;\\4", tableContentLines[index_start],ignore.case = TRUE)
        if (title_and_pageNumber==tableContentLines[index_start]) #this line is not the end of the title as expected
        {
          print("rest of the title in next lines is not conform to expected format")
          print(tableContentLines[index_start])
          feedback_dt  <<- rbind(feedback_dt,list(fileName,0, "", "page number issue due to format is not resolved; setting page number to 0 "))
          dt_content_table[i]$page = 0
        } else {
          # collect the page number and update
          title_and_pageNumber = unlist(strsplit(title_and_pageNumber,";;"))
          section_title <- paste(section_title, trimws(title_and_pageNumber[1]), sep= " ")
          dt_content_table[i]$title = section_title
          dt_content_table[i]$page = title_and_pageNumber[2]
        }
      } else # no title between the line to correct and the next title
      {
        print("there is no line  in TOC before the next title to take the rest of the current title from")
        print(tableContentLines[index_start])
        feedback_dt  <<- rbind(feedback_dt,list(fileName,0, "", "page number issue due to format is not resolved; setting page number to 0 "))
        dt_content_table[i]$page = 0
      }
    }
    i = i+1
  }
  # process the last title in the TOC by setting page number to 0 to
  if ((dt_content_table[i]$page < dt_content_table[i-1]$page))
  {
    print("rest of the title in next lines is not conform to expected format")
    print(tableContentLines[i])
    feedback_dt  <<- rbind(feedback_dt,list(fileName,0, "", "rest of the title in next lines is not conform to expected format"))
    
  }
  
  return(dt_content_table)
}
resolve_error_page_number <- function(fileName,txt, dt_content_table)
{
  # this function looks over the titles that have an erorr in the page number
  # it locates the page of the title before the problematic title and looks for the title with the issue until it finds it
  # then it extracts the page and assign it to the page with error.
  # from previous step the page with an error has been assigned page number 0
  
  #locate titles with Error issue, previously assigend the vlaeu 0 to page number
  dt_content_table_sub = which( dt_content_table$page == 0)
  
  
  if (length(dt_content_table_sub)==0)
  {return(dt_content_table)} # no error in page number
   
  
  #locate a page at the end of the table of contents TOC to start searching for the page of the title in case the page error happened at the first title in the TOC
  TOC_end_page = grep("(References?|(List of tables)|(list of Figures)|(Table\\s*Page)|(Figure\\s*Page)|(APPENDICES)|(Attachment))",txt, ignore.case = T, perl=T) 
  if (length(TOC_end_page)!=0)
  {
    TOC_end_page = TOC_end_page[1]
  }else {
    TOC_end_page = 1
  }
  
  i=1
  if(dt_content_table_sub[i]!=1)
  { 
    page_ref_index = dt_content_table_sub[i]-1 #start from the previous known indexed page; this is index in dt_content_table
    page_ref = as.numeric(dt_content_table[page_ref_index]$page)
    
   } else
   {
     page_ref = as.numeric(TOC_end_page) #in the case the Error in page title starts from the first title, we start searching right after the TOC section
   }
   
  for (i in 1:length(dt_content_table_sub))
  {
    #process each error found in index 
    #locate the page of the previous title with correct page number
    # if the error happened from the first title, then start from the first page that has table of content

    while(as.numeric(page_ref)<=length(txt))
    {
      if (is.na(locate_title_in_page(fileName, dt_content_table[dt_content_table_sub[i]]$level_number, dt_content_table[dt_content_table_sub[i]]$title,  page_ref, txt,0,dt_content_table[dt_content_table_sub[i]]$title_start)))
      {
        #move to the next page in the txt to try to find the page with the title
        page_ref = as.numeric(page_ref)+1
      } else {
        break
      }
    }
    if (as.numeric(page_ref)>length(txt))
    {
      sprintf ("page ERROR not resolved for: %s",dt_content_table[dt_content_table_sub[i]]$title)
      feedback_dt  <<- rbind(feedback_dt,list(fileName,dt_content_table[dt_content_table_sub[i]]$level_number, dt_content_table[dt_content_table_sub[i]]$title, "page ERROR not resolved"))
      return(dt_content_table)
    }
    else {
      dt_content_table[dt_content_table_sub[i]]$page = as.character(page_ref)
      feedback_dt  <<- rbind(feedback_dt,list(fileName,dt_content_table[dt_content_table_sub[i]]$level_number, dt_content_table[dt_content_table_sub[i]]$title, "page ERROR resolved"))
    }
    i=i+1
    if (i< length(dt_content_table_sub ))
    {
      page_ref_index = dt_content_table_sub[i]-1 # move one title before the title that has issues
      page_ref = as.numeric(dt_content_table[page_ref_index]$page)
    }
  }
   return(dt_content_table)
}
    
extract_title_content <- function(fileName,txt, dt_content_table, level_input,footer_index) 
{
      dt_content_table_sub = which( dt_content_table$level == level_input)
      dt_content_sections = data.table(Filename=character(),level=character(), level_number=character(), title=character(), content=character())
      #locate the first page where the content is 
      # we assume that the have at least 2 titles one of them stands for the stop stitle like references

      i=1
      #we process all the titles expect the sections about references, list of tables, list of figures
      while ((i<=length(dt_content_table_sub))  && (length(grep("(References?|(List of tables)|(list of Figures)|(Table\\s*Page)|(Figure\\s*Page)|(APPENDICES)|(Attachment))",dt_content_table[dt_content_table_sub[i]]$title, ignore.case = T, perl=T))==0))
      # the second condition is to avoid processing the title that correspond to the last element in table of content
      {
        current_sect_page = as.numeric(dt_content_table[dt_content_table_sub[i]]$page)
        current_lev_number = dt_content_table[dt_content_table_sub[i]]$level_number
        current_title=dt_content_table[dt_content_table_sub[i]]$title
        current_title_start=dt_content_table[dt_content_table_sub[i]]$title_start
 
        temp2 = character()
        
        #location of titles relative to a page
        current_title_loc = locate_title_in_page(fileName, current_lev_number,current_title, current_sect_page, txt,1,current_title_start)
        if (is.na(current_title_loc))
        {
          feedback_dt  <<- rbind(feedback_dt,list(fileName,current_lev_number, current_title,paste0("not able to locate ", current_lev_number," ", current_title," in text ")))
          log_error ("not able to locate title from TOC in text",fileName, length(txt))
          #files_not_processed <<- rbind(files_not_processed, list(fileName,"not able to locate title from TOC in text"))
          dt_content_sections = data.table(Filename=character(), level=character(), level_number=character(), title=character(), content=character())
          return(dt_content_sections)
          }
        temp_current <- unlist(strsplit(txt[current_sect_page],"\n"))
        
        #locate the title session that will be used to delimit the content of the current session
        next_i = dt_content_table_sub[i] + 1
        # test if next session exist; this situation happens if  the TOC did not include references title
        no_clear_TOC_end = FALSE
        if (next_i > nrow(dt_content_table))
        {
          no_clear_TOC_end= TRUE
        } else 
        {
          # test if all remaining non processed titles are more advanced titles, then we also  don't have a title delimiter 
          after_current_section = dt_content_table[next_i:nrow(dt_content_table)]
          after_current_section  = after_current_section[after_current_section$level<=level_input,]
          if(nrow(after_current_section) ==0)
          { no_clear_TOC_end= TRUE }
        }
        if (no_clear_TOC_end)
          #there is no tilte section that delimits this section, the content will be extracted until reaching the end of the document
        {
          # this is typically the case where there was no references section detected in the TOC stopped with list of tables, figures, etc.
          temp2 = paste(c(temp2, temp_current[current_title_loc: (length(temp_current)- footer_index)]), collapse= "\n")
          #read the pages until the end of the file  and add them to the content; loop through the pages ot remove hearder and footer
          current_sect_page = current_sect_page + 1
          while (current_sect_page <= length(txt))
          {
            temp_current <- unlist(strsplit(txt[current_sect_page],"\n"))
            temp2 = paste(c(temp2, temp_current[2: (length(temp_current)-1)]), collapse= "\n")
            current_sect_page = current_sect_page + 1
          }
          dt_content_sections = rbind(dt_content_sections, list(fileName,level_input,current_lev_number,current_title,temp2))
        
          break
        } # end of there is no tilte section that delimits this section, the content will be extracted until reaching the end of the document
          
        
        
        next_sect_page = as.numeric(after_current_section[1]$page)
        next_lev_number=after_current_section[1]$level_number
        next_title=after_current_section[1]$title
        next_title_start = after_current_section[1]$title_start
        #location of next titles relative to a page
        next_title_loc = locate_title_in_page(fileName, next_lev_number,next_title, next_sect_page, txt,1,next_title_start)
        

        if (is.na(current_title_loc) || is.na(next_title_loc))
        {
          feedback_dt  <<- rbind(feedback_dt,list(fileName,current_lev_number, current_title, "title or its next title delimiter are not found as verbatim in text"))
          print(paste0("title: ", next_lev_number," ", next_title, "or its next title delimiter are not found as verbatim in text"))
          temp2=""
          # an example where the title in TOC "Medical institution, investigator" and in the text is "Medical institution, principal investigator"
          # the content of this sesction will be empty
          # another example is "Allocation of the test drug to subjects" in TOC  and  Allocation of the investigational drug to subjects
          # when the title is also next to a title of the same level, both content are not found
        } else {
          if (current_sect_page == next_sect_page)
          {
            # the start of the next section determines the end of current session
            
            temp2 = paste(c(temp2, temp_current[current_title_loc: (next_title_loc-1)]), collapse= "\n")
          } else {
            
            #get the content of the current page that has the title , remove the footer information
            temp2 = paste(c(temp2, temp_current[current_title_loc: (length(temp_current)- footer_index)]), collapse= "\n")
            #continue through all pages until the next section 
            current_sect_page = current_sect_page +1
            while( current_sect_page != next_sect_page)
            {
              temp_current <- unlist(strsplit(txt[current_sect_page],"\n"))
              temp2 = paste(c(temp2, temp_current[2: (length(temp_current)- footer_index)]), collapse= "\n") #remove first and last line:header and footer
              current_sect_page = current_sect_page +1
            }
            #add the text before the next section title
            if(next_title_loc>2) #if the title of the next section start at the top of the page; then no content to extract from this page
            {
              temp_current <- unlist(strsplit(txt[current_sect_page],"\n"))
              temp2 = paste(c(temp2, temp_current[2: (next_title_loc-1)]), collapse= "\n") #remove the header and stop at the next title
            }
          } #end of current and next section are not in the same page
        }#end found title location 
        
        dt_content_sections = rbind(dt_content_sections, list(fileName,level_input,current_lev_number,current_title,temp2))
        i=i+1
      }#end of moving through the titles in the content table 
      
      return(dt_content_sections)
}

detect_footer_in_text <- function(dt_content_table, txt)
{
  i = 0
  first_page_processed = as.numeric(dt_content_table[1]$page)
  first_page_content = unlist(strsplit(txt[first_page_processed],"\n"))
  percent_pages_with_footer = 0.5*last_page_processed  #at least 50 of the document need to ahve the footer
  last_page_processed = as.numeric(dt_content_table[nrow(dt_content_table)]$page)
  
  found_footer = TRUE
  while (found_footer==TRUE)
    {
      footer  = first_page_content[(length(first_page_content)-i)]
      footer = gsub("\\s+"," ",footer)
      count_pages = 1
      j =first_page_processed +1
      while (j < last_page_processed  && (count_pages < percent_pages_with_footer))
        {
          current = unlist(strsplit(txt[j],"\n"))
          footer_current = current[(length(current)-i)]
          footer_current = gsub("\\s+"," ",footer_current)
          if (trimws(footer) == trimws(footer_current))
          {count_pages = count_pages + 1
          } else {break}
          j = j + 1
      }
      if (count_pages < percent_pages_with_footer)
      {found_footer = FALSE
      } else {i = i+1}
      
  }
 
  return(i)
}
escape_special_char_for_grepl_sub <- function(stringVar)
{
  escape_char=c(".","(",")","[","]")
  for (word in escape_char)
  {
    stringVar = gsub(word,paste0("\\",word,collapse=""),stringVar,fixed=TRUE)
  }
  return(stringVar)
}
 locate_title_in_page <- function(fileName, level_number, title,  sect_page, txt,report_error, title_start)
 {
   #report_error  is set to 1 when called from extract_title_content
   #otherwise it is set to other when called by resolve_error_page_number
   #locate_title_in_page(next_lev_number,next_title, next_sect_page, txt)
   # to find the title in the page we need to find the number and then check that the title is a substirng of the title from the content 

   
   content= unlist(strsplit(txt[sect_page],"\n"))
   lev_number = escape_special_char_for_grepl_sub(level_number) #need to replace . ( ) by preceding with \\ for grep to work properly
   titl_start = escape_special_char_for_grepl_sub(title_start) #need to replace . ( ) by preceding with \\ for grep to work properly 
   titl = escape_special_char_for_grepl_sub(title_start) #need to replace . ( ) by preceding with \\ for grep to work properly 
  
   
   
   title_lev_loc= which(grepl(paste0("(^\\s*)",lev_number,"(\\s+|[A-Z]+)",collapse=""),content, fixed=FALSE)) #title can be etiher spaced from title number or directly atached to it
   if (length(title_lev_loc)==0) # title number can be image - so not processed by conversion; test if we can find exact title in start title or full title; no partial matching 
   {
     part_text_found = which(grepl(paste0("(^\\s*)","((",titl_start,")|(",titl,"))","\\s*\\.*$",collapse=""),content, fixed=FALSE)) #if dot was part of the title it was removed when extracting title from TOC
     if (length(part_text_found) == 0) # did no find section number and title
     {
       if(report_error==1) {feedback_dt  <<- rbind(feedback_dt,list(fileName,level_number, title, "no matching of the title section number and title from TOC in text"))}
       return(part_text_found[1])
     } else #found title wihout title number
     {
       if(report_error==1) {feedback_dt  <<- rbind(feedback_dt,list(fileName,level_number, title, "no matching of the title section number but  found exact title text; probably title number is image"))}
       return(part_text_found[1])
     }
   } else # title number found; need to match title either exact match or partially
    {
      part_text = trimws((sub(paste0("^\\s*",lev_number,"\\s*(.+)$",collapse=""),"\\1", content[title_lev_loc[1]])))
      if (substr(part_text,nchar(part_text),nchar(part_text))==".") # if the title ended with "." the dot was removed when processing the TOC; we need to remove it from the title in the text
      {part_text = substr(part_text,1,(nchar(part_text)-1))}
      title_found = grep(part_text, gsub("\\.",".",title,fixed=TRUE), fixed=TRUE) #need to revert back to . for \\. for grep to work
      if (length(title_found)==0) {title_found = grep(part_text, gsub("\\.",".",(title_start),fixed=TRUE), fixed=TRUE)} #check if the title is part of he first line of the title or the ful title
      if (length(title_found )!=0)
      { 
        return(title_lev_loc[1])
      }else #title from TOC is not matching title in text
      {
        #search if the title in the TOC partially match the title in the text
       if (partial_match_title(part_text,title) || partial_match_title(part_text,title_start))
       { 
         if(report_error==1) {feedback_dt  <<- rbind(feedback_dt,list(fileName,level_number, title, "partial matching of title from TOC in text"))}
         return(title_lev_loc[1])
       } else {
         if(report_error==1) {feedback_dt <<- rbind(feedback_dt,list(fileName,level_number, title, "no matching of at least 10% of title from TOC in text"))}
        return(title_found[1])
        }
      }
     } 
     
 }
 partial_match_title <- function(part_text, title )
 {
   if (length(agrep(part_text, title,max = 0.5, ignore.case = TRUE, value = TRUE)) !=0)
    {return(TRUE)
   } else {return(FALSE)}
 }
extract_pdf_to_text <- function(fileDirInput,fileName)
{
  txt=""
  fl = file.path(fileDirInput,fileName)
  tryCatch(txt<- pdf_text(fl),error = function(w){print("not able to convert file")})
  return(txt)
}
extract_word_to_text <- function(fileDirInput,fileName)
{
  
  fl = file.path(fileDirInput,fileName)
  txt <- readtext(system.file(fl, package="docxtractr"))
  return(txt)
}

extract_titleSections_and_contentSections <- function(fileName,txt)
{
  dt_content_table = data.table(level=character(), level_number=character(), title=character(), page=numeric(),index_in_TOC=character(), title_start = character())
  dt_content_sections = data.table(Filename=character(), level=character(), level_number=character(), title=character(), content=character())
  dt_content_table = extract_TableOfContents(fileName, txt)
  
  if (nrow( dt_content_table)!=0)
  {
    dt_content_table = resolve_error_page_number(fileName,txt, dt_content_table)
    #footer_index = detect_footer_in_text(dt_content_table, txt)  need debugging
    #write titles to a file for debugging
    current_titles =dt_content_table[,c(2,3)]
    #View(current_titles)
    #View(dt_content_table)
    current_titles$Filename=fileName
    current_titles=current_titles[,c(3,1,2)]
    dt_all_section_titles <<- rbind(dt_all_section_titles, current_titles)
    
    footer_index = 0
    level_number = 1
    while (level_number < 4)
      {
      dt_content_sections = rbind(dt_content_sections, extract_title_content(fileName,txt, dt_content_table, level_number,footer_index))
      level_number = level_number +1
      if (nrow(dt_content_sections)==0)
      {
        #not able to locate previous titles
        break
      }
      #View(dt_content_sections)
      }

    #View(dt_content_sections)
  }

return(dt_content_sections)
}


files_not_processed = data.table(fileName=character(), comment=character())
feedback_dt  = data.table(Filename=character(), level_number=character(), title=character(), comment=character())
dt_all_section_titles = data.table(Filename=character(), level_number=character(), title=character()) #used to log TOC titles of all files for debugging


list_not_desired_keywords_inFileName = read_non_desired_keywords()
nb_header_lines = 0 #will be updated by extract_TableOfContents
nb_footer_lines = 0 #will be updated by extract_TableOfContents

count_files=0
setTimeLimit(cpu = Inf) # to force the code to run after efault 30mn
fileDirInput <- dirname(fileInput)
fileName <- basename(fileInput)


  if (!(grepl("(\\.pdf|\\.doc|\\.docx)", fileName,ignore.case = TRUE)) || (grepl("Addend", fileName,ignore.case = TRUE))){
    print("fileName without pdf/doc/docx extension or including addend keyword")
    files_not_processed <<- rbind(files_not_processed, list(fileName,"fileName without pdf/doc/docx extension or including addend keyword"))
  }else{
    if (grepl( "\\.pdf", fileName))
    {
      fileName_withNoExtension <- gsub("(.+)\\.pdf", "\\1",  fileName,ignore.case = TRUE )
      txt  = extract_pdf_to_text(fileDirInput, fileName)
    }
    else {
      fileName_withNoExtension <- gsub("(.+)\\.(doc|docx)", "\\1",  fileName,ignore.case = TRUE )
      txt = extract_word_to_text(fileDirInput, fileName)
    }
    #process the text files to extract sections and their titles
    if (!is_empty_txt(txt) && !is_non_english_txt(txt))
    {
      
      dt_content_sections = data.table(Filename=character(), level=character(), level_number=character(), title=character(), content=character())
      dt_content_sections = extract_titleSections_and_contentSections(fileName,txt)
       if (nrow(dt_content_sections) !=0)
       {
         x <- toJSON(setNames(as.data.frame(dt_content_sections),c("Filename","level","level_number","title","content")))
         write(x, stdout())
      }
    }
  }


feedback_dt =unique(feedback_dt)


  


handler <- function(x) {
    return(x + 1)
}

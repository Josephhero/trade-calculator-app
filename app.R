#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(nflverse)
library(gt)
library(gtExtras)
library(janitor)

# Needed to get download to work with {gt}
# From this issue: https://community.rstudio.com/t/how-to-properly-configure-google-chrome-on-shinyapps-io-because-of-webshot2/109020/4
library(webshot2)
library(pagedown)
library(curl)


all_teams <- load_teams()

baldwin_charts <- nflreadr::csv_from_url("https://github.com/nflverse/open-source-football/raw/master/_posts/2023-02-23-nfl-draft-value-chart/osf_draft_chart.csv")

reg_charts <- nflreadr::csv_from_url("https://github.com/nflverse/nfldata/raw/master/data/draft_values.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NFL Trade Chart Calculator"),
    splitLayout(cellWidths = c(140, 170), 
                cellArgs = list(style = "padding: 10px"),
                actionButton(inputId='twitter_id', label="@JosephJefe", 
                            icon = icon("twitter"),
                            onclick ="window.open('https://twitter.com/josephjefe')", 
                            style="color: white; background-color: #26a7de; border-color: #2e6da4"), 
               actionButton(inputId='coffee_id', label="Buy Me a Coffee", 
                            icon = icon("mug-hot"), 
                            onclick ="window.open('https://www.buymeacoffee.com/JosephJefe')", 
                            style="background-color: #ecb02e; border-color: #2e6da4")), 

    # Sidebar with variables 
    sidebarLayout(
      sidebarPanel(
        textInput(inputId = "title", 
                  label = "Custom Plot Title:"),
        h3("Trade Up Team"), 
        selectInput(inputId = "trade_up_team_id", 
                    label = "Team", 
                    choices = unique(all_teams$team_abbr), 
                    selected = "TEN"), 
        textInput(inputId = "trade_up_picks_id", 
                  label = "Picks Received (13, 197, Early 6)", 
                  value = "31, 178"), 
        helpText("Pick # or Early/Mid/Late {round}"), 
        textInput(inputId = "trade_up_years_id", 
                  label = "Year of each pick listed above", 
                  value = "2023, 2023"), 
        helpText("Number of years must match number of picks listed"), 
        h3("Trade Down Team"), 
        selectInput(inputId = "trade_down_team_id", 
                    label = "Team", 
                    choices = unique(all_teams$team_abbr), 
                    selected = "KC"), 
        textInput(inputId = "trade_down_picks_id", 
                  label = "Picks Received (33, Mid 4, Late 7)", 
                  value = "41, 186, Mid 2"), 
        helpText("Pick # or Early/Mid/Late {round}"), 
        textInput(inputId = "trade_down_years_id", 
                  label = "Year of each pick listed above", 
                  value = "2023, 2023, 2024"), 
        helpText("Number of years must match number of picks listed"), 
        sliderInput(inputId = "future_discount_id", 
                    label = "Future Pick Discount %", 
                    value = 10, 
                    min = 0, 
                    max = 50, 
                    step = 5), 
        radioButtons(inputId = "qb_trade_id", 
                     label = "QB Trade?", 
                     choices = c("Yes", "No"), 
                     selected = "No"), 
        submitButton(text = "Submit", 
                     icon = icon("refresh"),
                     width = NULL), 
        h5(), 
        downloadButton(outputId = "download", 
                       label = "Download", 
                       icon = icon("download")),
            
            
        ), 
        # Show a plot of the generated distribution
        mainPanel(
          gt_output("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  trade_table <- reactive({
    
    # Variables-----
    trade_up_team <- input$trade_up_team_id
    trade_down_team <- input$trade_down_team_id
    
    trade_up_picks <- str_squish(str_to_title(unlist(strsplit(input$trade_up_picks_id,","))))
    trade_up_years <- str_squish(str_to_title(unlist(strsplit(input$trade_up_years_id, ","))))
    
    trade_down_picks <- str_squish(str_to_title(unlist(strsplit(input$trade_down_picks_id,","))))
    trade_down_years <- str_squish(str_to_title(unlist(strsplit(input$trade_down_years_id, ","))))
    
    
    # Validate inputs-----
    
    trade_up_picks_length <- length(trade_up_picks)
    trade_up_years_length <- length(trade_up_years)
    
    up_words <- tibble(picks = trade_up_picks) |> 
      filter(!str_detect(trade_up_picks,"^\\s*[0-9]*\\s*$")) |> 
      separate(picks, into = str_squish(c("pick_word", "pick_num"))) |> 
      filter(!pick_word %in% c("Early", "Mid", "Late"))
    
    up_years <- tibble(year = trade_up_years) |> 
      filter(!year %in% 1945:2027)
    
    trade_down_picks_length <- length(trade_down_picks)
    trade_down_years_length <- length(trade_down_years)
    
    down_words <- tibble(picks = trade_down_picks) |> 
      filter(!str_detect(trade_down_picks,"^\\s*[0-9]*\\s*$")) |> 
      separate(picks, into = str_squish(c("pick_word", "pick_num"))) |> 
      filter(!pick_word %in% c("Early", "Mid", "Late"))
    
    down_years <- tibble(year = trade_down_years) |> 
      filter(!year %in% 1945:2027)
    
    err1 <- paste0("You have ", 
                   trade_up_picks_length, 
                   " draft picks in your trade up picks, but \n", 
                   trade_up_years_length, 
                   " years for those picks. \n", 
                   "The number of draft picks must match \n", 
                   "the number of years listed")
    
    err2 <- paste0('Your trade up picks were entered as: \n\n', 
                   input$trade_up_picks_id,
                   '\n\nPicks must be only numbers or ', 
                   'a pick in the format of \n\n"Early/Mid/Late {space} {round #}"\n\n', 
                   'For example, "22, Mid 2, Early 7".',
                   '\nPicks must also be comma separated.') 
    
    err3 <- paste0('Your years were entered as: \n\n', 
                   input$trade_up_years_id, 
                   '\n\nYears must be entered as YYYY, ', 
                   'and be comma separated.', 
                   '\nFor example, "2023, 2023, 2024"')
    
    err4 <- paste0("You have ", 
                   trade_down_picks_length, 
                   " draft picks in your trade down picks, but \n", 
                   trade_down_years_length, 
                   " years for those picks. \n", 
                   "The number of draft picks must match \n", 
                   "the number of years listed")
    
    err5 <- paste0('Your picks were entered as: \n\n', 
                   input$trade_down_picks_id,
                   '\n\nPicks must be only numbers or ', 
                   'a pick in the format of \n\n"Early/Mid/Late {space} {round #}"\n\n', 
                   'For example, "22, Mid 2, Early 7".',
                   '\nPicks must also be comma separated.')
    
    
    err6 <- paste0('Your years were entered as: \n\n', 
                   input$trade_down_years_id, 
                   '\n\nYears must be entered as YYYY, ', 
                   'and be comma separated. \n', 
                   'For example, "2023, 2023, 2024"')
    
    x_val <- case_when(trade_up_picks_length != trade_up_years_length ~ 1, 
                       nrow(up_words) > 0 ~ 2, 
                       nrow(up_years) > 0 ~ 3, 
                       trade_down_picks_length != trade_down_years_length ~ 4, 
                       nrow(down_words) > 0 ~ 5, 
                       nrow(down_years) > 0 ~ 6,
                       TRUE ~ 7)
    
    error_message <- case_when(x_val == 1 ~ err1, 
                               x_val == 2 ~ err2, 
                               x_val == 3 ~ err3, 
                               x_val == 4 ~ err4, 
                               x_val == 5 ~ err5, 
                               x_val == 6 ~ err6)
    
    validate(need(x_val == 7, error_message))    
    
    # Data-----
    trade_up_vars <- 
      tibble(pick_label = str_squish(str_to_title(unlist(strsplit(input$trade_up_picks_id,",")))),
             year       = str_squish(str_to_title(unlist(strsplit(input$trade_up_years_id, ","))))
      ) |> 
      mutate(team       = trade_up_team, .before = "pick_label") |> 
      mutate(rank       = as.character(row_number()))
    
    trade_down_vars <- 
      tibble(pick_label = str_squish(str_to_title(unlist(strsplit(input$trade_down_picks_id,",")))),
             year       = str_squish(str_to_title(unlist(strsplit(input$trade_down_years_id, ","))))
      ) |> 
      mutate(team       = trade_down_team, .before = "pick_label") |> 
      mutate(rank       = as.character(row_number()))
    
    
    trade_year <- min(as.numeric(trade_up_vars$year))
    

    future_discount <- input$future_discount_id
  
    
    #future_discount <- if_else(input$future_discount_id > 50, 50, input$future_discount_id)
    
    qb_trade <- input$qb_trade_id
    
    # Data-----
    
    charts1 <- left_join(reg_charts, 
                         select(baldwin_charts, draft_overall, baldwin = osf_surplus), 
                         by = c("pick" = "draft_overall")) |> 
      mutate_all(~replace_na(.,0))
    
    
    draft_picks1 <- load_draft_picks(seasons = 2018:2022) |> 
      select(season, round, pick)
    
    draft_picks2 <- draft_picks1 |> 
      group_by(round, season) |> 
      slice_head(n = 1) |> 
      ungroup() |> 
      group_by(round) |> 
      summarize(round_start = round(mean(pick))) |> 
      mutate(round_end = lead(round_start, default = max(charts1$pick) + 1) -1) |> 
      mutate(round_picks = round_end - round_start + 1) |> 
      mutate(round_early = round_start + floor(round_picks / 3) - 1) |> 
      mutate(round_late = round_end - floor(round_picks / 3) + 1)
    
    draft_picks3 <- draft_picks1 |> 
      select(round, pick) |> 
      distinct() |> 
      filter(pick <= max(charts1$pick)) |> 
      left_join(draft_picks2, by = "round") |> 
      slice_head(n = 262)
    
    draft_picks <- draft_picks3 |> 
      mutate(round_label = case_when(
        pick <= round_early ~ paste0("Early ", round), 
        pick >= round_late ~ paste0("Late ", round), 
        TRUE ~ paste0("Mid ", round)
      )) |> 
      add_row(round = 7, pick = 263, round_label = "Late 7")
    
    draft_picks_labels <- draft_picks |> 
      group_by(round_label) |> 
      summarize(pick = trunc(mean(pick))) |> 
      ungroup()
    
    charts2 <- left_join(charts1, draft_picks_labels, by = "pick") 
    
    charts3 <- charts2 |> 
      filter(!is.na(round_label))
    
    charts <- bind_rows(select(charts2, -round_label), charts3) |> 
      arrange(pick) |> 
      mutate(pick_label = coalesce(round_label, as.character(pick))) |> 
      add_row(pick = 263, stuart = 0, johnson = 0, hill = 0, otc = 0, pff = 0, baldwin = 0)
    
    
    #trade_up_df <- charts_raw |> 
    trade_up_df <- charts |> 
      right_join(trade_up_vars, by = "pick_label") |> 
      mutate(pick = pick_label, .before = "stuart") |> 
      select(-pick_label, -round_label) |> 
      mutate(total = "Pick", .before = "pick") |> 
      mutate(stuart = case_when(
        year > trade_year ~ round(stuart * ((100 - future_discount) / 100), 1), 
        TRUE ~ stuart
      )) |> 
      mutate(johnson = case_when(
        year > trade_year ~ round(johnson * ((100 - future_discount) / 100), 0), 
        TRUE ~ johnson
      )) |> 
      mutate(hill = case_when(
        year > trade_year ~ round(hill * ((100 - future_discount) / 100), 2), 
        TRUE ~ hill
      )) |> 
      mutate(otc = case_when(
        year > trade_year ~ round(otc * ((100 - future_discount) / 100), 0), 
        TRUE ~ otc
      )) |> 
      mutate(pff = case_when(
        year > trade_year ~ round(pff * ((100 - future_discount) / 100), 3), 
        TRUE ~ pff
      )) |> 
      mutate(baldwin = case_when(
        year > trade_year ~ round(baldwin * ((100 - future_discount) / 100), 0), 
        TRUE ~ baldwin
      )) |> 
      arrange(rank) |> 
      janitor::adorn_totals("row",,,, c(stuart:baldwin), fill = "") |> 
      mutate(team = trade_up_team, .before = "total") |> 
      mutate(received = paste0(trade_up_team, " RECEIVED:"), .before = "total") |> 
      mutate(year = as.character(year), .after = "pick")
    
    
    #trade_down_df <- charts_raw |> 
    trade_down_df <- charts |> 
      right_join(trade_down_vars, by = "pick_label") |> 
      mutate(pick = pick_label, .before = "stuart") |> 
      select(-pick_label, -round_label) |> 
      mutate(total = "Pick", .before = "pick") |> 
      mutate(stuart = case_when(
        year > trade_year ~ round(stuart * ((100 - future_discount) / 100), 1), 
        TRUE ~ stuart
      )) |> 
      mutate(johnson = case_when(
        year > trade_year ~ round(johnson * ((100 - future_discount) / 100), 0), 
        TRUE ~ johnson
      )) |> 
      mutate(hill = case_when(
        year > trade_year ~ round(hill * ((100 - future_discount) / 100), 2), 
        TRUE ~ hill
      )) |> 
      mutate(otc = case_when(
        year > trade_year ~ round(otc * ((100 - future_discount) / 100), 0), 
        TRUE ~ otc
      )) |> 
      mutate(pff = case_when(
        year > trade_year ~ round(pff * ((100 - future_discount) / 100), 3), 
        TRUE ~ pff
      )) |> 
      mutate(baldwin = case_when(
        year > trade_year ~ round(baldwin * ((100 - future_discount) / 100), 0), 
        TRUE ~ baldwin
      )) |> 
      arrange(rank) |> 
      janitor::adorn_totals("row",,,, c(stuart:baldwin), fill = "") |> 
      mutate(team = trade_down_team, .before = "total") |> 
      mutate(received = paste0(trade_down_team, " RECEIVED:"), .before = "total") |> 
      mutate(year = as.character(year), .after = "pick") 
    
    excess1 <- bind_rows(trade_down_df, trade_up_df) |> 
      filter(total == "Total") |> 
      #select(pff:pff)
      select(stuart:baldwin)
    
    excess2 <- excess1[1, ] - excess1[2, ]
    
    excess3 <- excess2 |> 
      mutate(team = trade_down_team, .before = "stuart") |> 
      mutate(received = paste0(trade_down_team, " EXCESS VALUE"), .before = "stuart") |> 
      mutate(total = "Point") |> 
      mutate(pick = "Diff")
    
    excess_pick1 <- excess3 |> 
      select(team, received) |> 
      distinct() |> 
      mutate(stuart = min(charts$pick[which(charts$stuart < if_else(
        excess3$stuart >= 0, excess3$stuart, excess3$stuart * -1, missing = 0))])) |> 
      mutate(johnson = min(charts$pick[which(charts$johnson < if_else(
        excess3$johnson >= 0, excess3$johnson, excess3$johnson * -1, missing = 0))])) |> 
      mutate(hill = min(charts$pick[which(charts$hill < if_else(
        excess3$hill >= 0, excess3$hill, excess3$hill * -1, missing = 0))])) |> 
      mutate(otc = min(charts$pick[which(charts$otc < if_else(
        excess3$otc >= 0, excess3$otc, excess3$otc * -1, missing = 0))])) |> 
      mutate(pff = min(charts$pick[which(charts$pff < if_else(
        excess3$pff >= 0, excess3$pff, excess3$pff * -1, missing = 0))])) |> 
      mutate(baldwin = min(charts$pick[which(charts$baldwin < if_else(
        excess3$baldwin >= 0, excess3$baldwin, excess3$baldwin * -1, missing = 0))])) |> 
      mutate(total = "Excess", .after = "received") |> 
      mutate(pick = "Pick", .after = "total") 
    
    excess_label <- excess_pick1 |> 
      pivot_longer(cols = c(stuart:baldwin)) |> 
      left_join(select(draft_picks, pick, round_label), by = c("value" = "pick")) |> 
      select(-value) |> 
      pivot_wider(names_prefix = "x", names_from = name, values_from = round_label) |> 
      mutate(pick_num = paste0(total, " ", pick), .before = pick) |> 
      select(-pick, -total)
    
    excess_pick <- excess_pick1 |> 
      mutate(stuart = if_else(excess3$stuart < 0, stuart - (stuart * 2), stuart)) |> 
      mutate(johnson = if_else(excess3$johnson < 0, johnson - (johnson * 2), johnson)) |> 
      mutate(hill = if_else(excess3$hill < 0, hill - (hill * 2), hill)) |> 
      mutate(otc = if_else(excess3$otc < 0, otc - (otc * 2), otc)) |> 
      mutate(pff = if_else(excess3$pff < 0, pff - (pff * 2), pff)) |> 
      mutate(baldwin = if_else(excess3$baldwin < 0, baldwin - (baldwin * 2), baldwin))
    
    if(qb_trade == "No"){
      trades <- bind_rows(trade_up_df, trade_down_df, excess3, excess_pick) |> 
        relocate(year, .after = "pick") |> 
        mutate(pick_num = case_when(
          nchar(pick) <= 4 ~ str_squish(paste0(total, " ", pick)),
          TRUE ~ pick),
          .before = pick) |>
        select(-total, -pick) |> 
        left_join(excess_label, by = c("team", "received", "pick_num")) 
    } else {
      trades <- bind_rows(trade_up_df, trade_down_df, excess3, excess_pick) |> 
        relocate(year, .after = "pick") |> 
        mutate(pick_num = case_when(
          nchar(pick) <= 4 ~ str_squish(paste0(total, " ", pick)),
          TRUE ~ pick),
          .before = pick) |>
        select(-total, -pick) |> 
        left_join(excess_label, by = c("team", "received", "pick_num")) |> 
        select(-baldwin, -xbaldwin) |> 
        mutate(baldwin = 0, .after = "pff") |> 
        mutate(xbaldwin = NA, .after = "xpff")
    }
    
    
    
    teams = load_teams() |> 
      select(team_abbr, team_logo_espn) |> 
      filter(team_abbr %in% c(trade_up_team, trade_down_team))
    
    team_urls <- left_join(select(trades, team, received), teams, by = c("team" = "team_abbr")) |> 
      select(-team) |> 
      distinct() |> 
      tibble::deframe()
    
    
    trade_up_team_logo <- teams |> 
      filter(team_abbr == trade_up_team)
    
    trade_down_team_logo <- teams |> 
      filter(team_abbr == trade_down_team)
    
    # Table-----
    
    merge_pattern <- '{1}<br><small><small>{2}</small></small>'
    
    tab <- 
      gt(trades, 
         groupname_col = c("received")) |> 
      gt_theme_538() |> 
      cols_hide(c(team, rank)) |> 
      cols_label(#total = "", 
        pick_num = "") |> 
      sub_missing(columns = everything(), rows = everything(), missing_text = "") |>
      cols_merge(
        columns = c(stuart, xstuart),
        pattern = merge_pattern
      ) |>
      cols_merge(
        columns = c(johnson, xjohnson),
        pattern = merge_pattern
      ) |>
      cols_merge(
        columns = c(hill, xhill),
        pattern = merge_pattern
      ) |>
      cols_merge(
        columns = c(otc, xotc),
        pattern = merge_pattern
      ) |>
      cols_merge(
        columns = c(pff, xpff),
        pattern = merge_pattern
      ) |> 
      cols_merge(
        columns = c(baldwin, xbaldwin),
        pattern = merge_pattern
      ) |>
      # Header text and format
      tab_header(title = ifelse(input$title != "", input$title, 
                                paste0(trade_up_team, 
                                       " Trades Up with ", 
                                       trade_down_team, 
                                       " for Pick ", 
                                       trade_up_vars$pick_label[1])), 
                 subtitle = paste0("Future picks discounted by ", 
                                   future_discount, 
                                   "%")
      ) |> 
      tab_style(
        style = list(
          cell_text(weight = "bold", 
                    align = "center")
        ),
        locations = cells_title(groups = c("title", "subtitle"))
      ) |> 
      tab_style(
        style = list(
          cell_text(weight = "bold", 
                    size = "14px")
        ),
        locations = cells_row_groups(groups = everything())
      ) |> 
      tab_options(row_group.padding = px(-30)) |> 
      tab_style(
        style = cell_text(indent = px(30)), 
        locations = cells_body(columns = pick_num)
      ) |>
      data_color(columns = stuart,
                 rows = pick_num == "Total",
                 palette = c("#af8dc3", "#7fbf7b")) |>
      data_color(columns = johnson,
                 rows = pick_num == "Total",
                 palette = c("#af8dc3", "#7fbf7b")) |>
      data_color(columns = hill,
                 rows = pick_num == "Total",
                 palette = c("#af8dc3", "#7fbf7b")) |>
      data_color(columns = otc,
                 rows = pick_num == "Total",
                 palette = c("#af8dc3", "#7fbf7b")) |>
      data_color(columns = pff,
                 rows = pick_num == "Total",
                 palette = c("#af8dc3", "#7fbf7b")) |>
      data_color(columns = baldwin,
                 rows = pick_num == "Total",
                 palette = c("#af8dc3", "#7fbf7b")) |>
      cols_width(c("stuart", "johnson", "hill", "otc", "pff", "baldwin") ~ px(60)) |>
      # Column Label format
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(year:baldwin)
      ) |>
      tab_style(
        style = cell_text(color = "#1b7837", weight = "bold"),
        locations = cells_body(
          columns = c(stuart:baldwin),
          rows = c(pick_num %in% c("Point Diff", "Excess Pick"))
        )
      ) |>
      tab_style(
        style = cell_text(color = "#762a83", weight = "bold", style = "italic"),
        locations = cells_body(
          columns = stuart,
          rows = stuart <= 0
        )
      ) |>
      tab_style(
        style = cell_text(color = "#762a83", weight = "bold", style = "italic"),
        locations = cells_body(
          columns = johnson,
          rows = johnson <= 0
        )
      ) |>
      tab_style(
        style = cell_text(color = "#762a83", weight = "bold", style = "italic"),
        locations = cells_body(
          columns = hill,
          rows = hill <= 0
        )
      ) |>
      tab_style(
        style = cell_text(color = "#762a83", weight = "bold", style = "italic"),
        locations = cells_body(
          columns = otc,
          rows = otc <= 0
        )
      ) |>
      tab_style(
        style = cell_text(color = "#762a83", weight = "bold", style = "italic"),
        locations = cells_body(
          columns = pff,
          rows = pff <= 0
        )
      ) |>
      tab_style(
        style = cell_text(color = "#762a83", weight = "bold", style = "italic"),
        locations = cells_body(
          columns = baldwin,
          rows = baldwin <= 0
        )
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(
          columns = everything(), 
          rows = pick_num == "Total"
        )
      ) |>
      tab_style(
        style = list(
          cell_borders(
            side = c("top", "bottom"),
            color = "black",
            weight = px(2)
          )
        ),
        locations = cells_body(
          columns = everything(), 
          rows = pick_num == "Total"
        )
      ) |>
      fmt_number(columns = stuart:pff,
                 rows = pick_num == "Excess Pick",
                 decimals = 0,
                 drop_trailing_zeros = TRUE
      ) |>
      gt::text_transform(
        locations = cells_row_groups(),
        fn = function(x) {
          lapply(x, function(x) {
            gt::html(paste(
              web_image(
                url = team_urls[[x]],
                #url = up_team[[x]], 
                height = 50
              ),
              '<div style = "display:flex;align-items:center;">', x, '</div>'
            ))
          })
        }
      ) |> 
      fmt(
        columns = baldwin,
        fns = function(x) ifelse(x == 0, "—", x)
      ) |> 
      tab_footnote(
        footnote = paste0("Baldwin chart should not be used to measure a trade up for a QB."),
        locations = cells_column_labels(
          columns = baldwin
        )
      ) |>
      tab_source_note(
        source_note = md(
          "<div style=\"width: 100%; display: table;\">
              <div style=\"display: table-row\">
                  <div style=\"width: 80%; display: table-cell;\">
                    <img src=\"https://github.com/Josephhero/Jefe-Logo/raw/main/Jefe%20Logo%20Skyline.png\" style=\"height:25px;\">
                  </div>
                  <div style=\"display: table-cell;vertical-align: middle;\">Data: nflverse.com</div>
              </div>
          </div>"
        )
    )
  })


  # Output----------------------------------------------------------------------
  
  # Needed to get download to work with {gt}
  # From this issue: https://community.rstudio.com/t/how-to-properly-configure-google-chrome-on-shinyapps-io-because-of-webshot2/109020/4
  message(curl::curl_version()) # check curl is installed
  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    chromote::set_default_chromote_object(
      chromote::Chromote$new(chromote::Chrome$new(
        args = c("--disable-gpu",
                 "--no-sandbox",
                 "--disable-dev-shm-usage", # required bc the target easily crashes
                 c("--force-color-profile", "srgb"))
      ))
    )
  }
  # End of Chromote Code 
  
  
  output$table <- render_gt({
    trade_table() 
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste0(input$trade_up_team_id,
             " Trades Up with ",
             input$trade_down_team_id,
             " for Pick ",
             str_squish(str_to_title(unlist(strsplit(input$trade_down_picks_id,","))))[1],
             ".png")
    },
    content = function(file) {gtsave(trade_table(), file = file)},
    contentType = "image/png"
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

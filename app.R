#    ---------------------------------------------------------------------    #
#                                                                             #
#   App:    "Surf Stat" Statistics Visualizer: Normal and t-distributions     #
#                                                                             #
#   ----------------------------------------------------------------------    #
#                                                                             #
#   Author: Xander Posner, MPH '20 | Epidemiology & Biostatistics             #
#           Instructional Designer, Online/On-Campus MPH Program (OOMPH)      #
#           UC Berkeley School of Public Health                               #
#           xander@berkeley.edu                                               #
#                                                                             #
#   ----------------------------------------------------------------------    #
#                                                                             #
#   Course: PBHLTH W142                                                       #
#           Statistics and Probability for Biology and Public Health          #
#           Professor Maureen Lahiff, Ph.D.                                   #
#           University of California, Berkeley                                #
#                                                                             #
#   ----------------------------------------------------------------------    #
#                                                                             #
#   Find out more about building applications with Shiny here:                #
#                                                                             #
#   https://shiny.rstudio.com/                                                #
#                                                                             #
#   ----------------------------------------------------------------------    #


# libraries required
library(shiny)
library(shinydashboard)
library(ggplot2)


################################# Norm Tab UI #################################


# 1. norm yellow Inputs box
norm_input_box <- box(
    title = "Inputs",
    status = "warning",
    solidHeader = TRUE,
    collapsible = TRUE,
    fluidRow(
        # Tail type radio buttons box
        box(
            radioButtons(
                "norm_tail",
                "Type",
                choices = c(
                    "Left-Tailed" = "left",
                    "Right-Tailed" = "right",
                    "Central Area" = "middle",
                    "Two-Tailed" = "two"
                ),
                selected = "left"
            )
        ),
        # Numeric inputs box
        box(
            # Z-score numeric input widget
            numericInput(
                "z",
                "Enter Z-Score",
                value = 1,
                min = NA,
                max = NA,
                step = 0.25
            ),
            # Arrow icons radio buttons
            radioButtons(
                "norm_arrow",
                NULL,
                choiceNames = list(icon("arrow-up"),
                                   icon("arrow-down")),
                choiceValues = list("up",
                                    "down"),
                inline = TRUE,
                selected = "down"
            ),
            # Area numeric inputs widget
            numericInput(
                "norm_area",
                "Area Under the Curve",
                value = 0.8413,
                min = 0,
                max = 1,
                step = 0.01
            )
        )
    )
)

# 2. norm blue Plot box
norm_plot_box <- box(
    plotOutput("normPlot"),
    title = "Plot",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE
)

# 3. norm first row
norm_row1 <- fluidRow(norm_input_box, norm_plot_box)

# 4. Normal Distribution tab
norm_tab <- tabItem(tabName = "norm",
                    h2("The Normal Distribution"),
                    norm_row1)


################################### t Tab UI ##################################


# 1. t yellow Inputs box
t_input_box <- box(
    title = "Inputs",
    status = "warning",
    solidHeader = TRUE,
    collapsible = TRUE,
    fluidRow(
        # Tail type radio buttons box
        box(
            radioButtons(
                "t_tail",
                "Type",
                choices = c(
                    "Left-Tailed" = "left",
                    "Right-Tailed" = "right",
                    "Central Area" = "middle",
                    "Two-Tailed" = "two"
                ),
                selected = "left"
            ),
            numericInput(
                "df",
                "Enter Degrees of Freedom",
                value = 1,
                min = 1,
                max = NA,
                step = 1
            )
        ),
        # Numeric inputs left box
        box(
            # T-statistic numeric input widget
            numericInput(
                "t",
                "Enter T-Statistic",
                value = 1,
                min = NA,
                max = NA,
                step = 0.25
            ),
            # Arrow icons radio buttons
            radioButtons(
                "t_arrow",
                NULL,
                choiceNames = list(icon("arrow-up"),
                                   icon("arrow-down")),
                choiceValues = list("up",
                                    "down"),
                inline = TRUE,
                selected = "down"
            ),
            # Area numeric inputs widget
            numericInput(
                "t_area",
                "Area Under the Curve",
                value = 0.8413,
                min = 0,
                max = 1,
                step = 0.01
            )
        )
    )
)

# 2. blue t Plot box
t_plot_box <- box(
    plotOutput("tPlot"),
    title = "Plot",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE
)

# 3. t first row
t_row1 <- fluidRow(t_input_box, t_plot_box)

# 4. t Distribution tab
t_tab <- tabItem(tabName = "t",
                 h2(uiOutput("t_header")),
                 t_row1)


################################ Source Tab UI ################################

# Source tab contact info box
source_contact_box <- box(
    title = "Author",
    p(uiOutput("contact")),
    collapsible = TRUE,
    collapsed = FALSE,
    solidHeader = TRUE,
    status = "success"
)

# Source tab R Source Code box
source_code_box <- box(
    title = "Code",
    uiOutput("code"),
    collapsible = TRUE,
    collapsed = FALSE,
    solidHeader = TRUE,
    status = "danger"
)

source_row1 <- verticalLayout(source_code_box, source_contact_box)

source_tab <- tabItem(tabName = "source",
                      h2("Project Info"),
                      source_row1)

################################ Dashboard UI #################################

header <- dashboardHeader(title = "Surf Stat",
                          titleWidth = "300px")

sidebar <- dashboardSidebar(sidebarMenu(
    menuItem(
        "The Normal Distribution",
        tabName = "norm",
        icon = icon("bell")
    ),
    menuItem(
        span("The ", em("t") , "-distribution"),
        tabName = "t",
        icon = icon("tshirt")
    ),
    menuItem(
        "Project Info",
        tabName = "source",
        icon = icon("laptop-code")
    )
))

# 16x16 pixel favicon (icon next to site name in web browser tab)
favicon <- titlePanel(
    title = tags$head(
        tags$link(rel="shortcut icon",
                  href="https://raw.githubusercontent.com/posnerab/surfstat/master/www/favicon.ico",
                  type="www/favicon.ico")))

body <- dashboardBody(favicon,
                      tabItems(norm_tab, t_tab, source_tab))

ui <- dashboardPage(header, sidebar, body)




################################ Server Logic #################################


server <- function(input, output, session) {
    xvalues <- data.frame(x = c(-3, 3))

    observe({
        # define input access objects
        z <-
            ifelse(
                input$norm_tail == "middle" | input$norm_tail == "two",
                ifelse(input$z == 0, 0.01, input$z),
                input$z
            )
        nt <- input$norm_tail
        na <- input$norm_arrow
        nu <- ifelse(
            input$norm_area > 0 & input$norm_area < 1,
            input$norm_area,
            ifelse(input$norm_area == 0 |
                       input$norm_area < 0, 0.01, 0.99)
        )




        #### NORMAL DISTRIBUTION SERVER LOGIC ####

        #### dnorm_tail ####
        # ggplot statistical function for shading area under Normal curve
        dnorm_tail <- reactive({
            req(z)
            req(nu)
            if (nt == "left") {
                function(x) {
                    norm_left <- dnorm(x)
                    norm_left[x <= -3 | x >= z] <- NA
                    return(norm_left)
                }
            }
            else if (nt == "right") {
                function(x) {
                    norm_right <- dnorm(x)
                    norm_right[x <= z | x >= 3] <- NA
                    return(norm_right)
                }
            }
            else if (nt == "middle") {
                function(x) {
                    norm_mid <- dnorm(x)
                    norm_mid[x <= -z | x >= z] <- NA
                    return(norm_mid)
                }
            }
            else if (nt == "two") {
                function(x) {
                    norm_two <- dnorm(x)
                    norm_two[x >= -z & x <= z] <- NA
                    return(norm_two)
                }
            }
        })

        #### norm_area_fun ####
        # function to compute area under t curve from t-statistic
        norm_area_fun <- reactive({
            req(z)
            req(nu)
            if (nt == "left") {
                round(pnorm(q = z,
                            lower.tail = TRUE),
                      digits = 5)
            }
            else if (nt == "right") {
                round(pnorm(q = z,
                            lower.tail = FALSE),
                      digits = 5)
            }
            else if (nt == "middle") {
                round(pnorm(q = z,
                            lower.tail = TRUE) -
                          pnorm(q = -z,
                                lower.tail = TRUE),
                      digits = 5)
            }
            else if (nt == "two") {
                round(
                    pnorm(q = z,
                          lower.tail = FALSE) +
                        pnorm(q = -z,
                              lower.tail = TRUE),
                    digits = 5
                )
            }
        })

        #### z_fun ####
        # function to compute t-statistic from area under the curve
        z_fun <- reactive({
            req(z)
            req(nu)
            if (nt == "left") {
                round(qnorm(p = nu,
                            lower.tail = TRUE),
                      digits = 5)
            }
            else if (nt == "right") {
                round(qnorm(p = nu,
                            lower.tail = FALSE),
                      digits = 5)
            }
            else if (nt == "middle") {
                round(qnorm(
                    p = (nu + ((
                        1 - nu
                    ) / 2)),
                    mean = 0,
                    sd = 1,
                    lower.tail = TRUE
                ),
                digits = 5)
            }
            else if (nt == "two") {
                round(qnorm(
                    p = nu / 2,
                    mean = 0,
                    sd = 1,
                    lower.tail = FALSE
                ),
                digits = 5)
            }
        })

        #### norm_area_value ####
        norm_area_value <- reactive({
            req(z)
            req(nu)
            if (na == "down") {
                c(round(norm_area_fun(), 5))
            }
            else if (na == "up") {
                if (nu > 0 & nu < 1) {
                    c(nu)
                }
                else if (nu == 0) {
                    c(0.01)
                }
                else if (nu == 1) {
                    c(0.99)
                }
            }
        })

        norm_area_label <- reactive({
            req(z)
            req(nu)
            if (na == "down") {
                c("Area Under the Curve")
            }
            else if (na == "up") {
                c("Enter Area Under the Curve (0 to 1)")
            }
        })

        z_value <- reactive({
            req(z)
            req(nu)
            if (na == "up") {
                if (nu > 0 & nu < 1 & z != 0) {
                    c(round(z_fun(), 5))
                }
                else {
                    c(1)
                }
            }
            else if (na == "down") {
                if (nt == "left" | nt == "right") {
                    c(z)
                }
                else if (nt == "middle" | nt == "two") {
                    if (z > 0) {
                        c(z)
                    }
                    else if (z < 0) {
                        c(-z)
                    }
                }
            }
        })

        z_label <- reactive({
            req(z)
            req(nu)
            if (na == "up") {
                c("Z-Score")
            }
            else if (na == "down") {
                if (nt == "left" | nt == "right") {
                    c("Enter Z-Score")
                }
                else if (nt == "middle" | nt == "two") {
                    c("Enter Z-Score (Positive)")
                }
            }
        })

        nu_min <- reactive({
            req(z)
            req(nu)
            if (na == "down") {
                c(round(norm_area_fun(), 5))
            }
            else if (na == "up") {
                c(0.00001)
            }
        })

        nu_max <- reactive({
            req(z)
            req(nu)
            if (na == "down") {
                c(round(norm_area_fun(), 5))
            }
            else if (na == "up") {
                c(0.99999)
            }
        })

        z_min <- reactive({
            req(nu)
            req(z)
            if (na == "up") {
                if (nt == "middle" | nt == "two") {
                    c(0.00001)
                }
                else {
                    c(round(z_fun(), 5))
                }
            }
            else if (na == "down") {
                if (nt == "left" | nt == "right") {
                    c(NA)
                }
                else if (nt == "middle" | nt == "two") {
                    c(0)
                }
            }
        })

        z_max <- reactive({
            req(nu)
            req(z)
            if (na == "up") {
                c(round(z_fun(), 5))
            }
            else if (na == "down") {
                c(NA)
            }
        })

        norm_plot_area_label <- reactive({
            req(z)
            req(nu)
            if ((round(norm_area_fun(), 4) * 100) < 0.01) {
                paste0("Area: ", c(
                    formatC(
                        norm_area_fun(),
                        format = "e",
                        digits = 2
                    )
                ))
            }
            else {
                paste0("Area: ", round(norm_area_fun(), 4) * 100, "%")
            }
        })


        updateNumericInput(
            session,
            "z",
            label = z_label(),
            value = z_value(),
            min = z_min(),
            max = z_max()
        )

        updateNumericInput(
            session,
            "norm_area",
            label = norm_area_label(),
            value = norm_area_value(),
            min = nu_min(),
            max = nu_max()
        )

        output$normPlot <- renderPlot({
            ggplot(xvalues, aes(x = xvalues$x)) +
                stat_function(fun = dnorm, size = .9) +
                stat_function(
                    fun = dnorm_tail(),
                    geom = "area",
                    fill = "red",
                    alpha = 0.3
                ) +
                labs(x = "\n Z-Score (z)",
                     y = "",
                     title = "Standard Normal Distribution \n") +
                geom_text(
                    x = 2.1,
                    y = 0.3,
                    size = 6,
                    fontface = "bold",
                    colour = "brown",
                    label = norm_plot_area_label()
                ) +
                geom_text(
                    x = 2.1,
                    y = 0.25,
                    size = 6,
                    fontface = "bold",
                    colour = "brown",
                    label = paste0("z: ",
                                   formatC(
                                       round(z, 4),
                                       format = "f",
                                       digits = 3
                                   ))
                ) +
                theme(
                    plot.title = element_text(
                        face = "bold",
                        size = 16,
                        hjust = 0.5
                    ),
                    axis.title.x = element_text(
                        face = "bold",
                        colour = "brown",
                        size = 14
                    ),
                    axis.title.y = element_text(
                        face = "bold",
                        colour = "brown",
                        size = 12
                    ),
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_blank()
                ) +
                scale_x_continuous(limits = c(-3, 3),
                                   breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
                scale_y_continuous(breaks = NULL)
        })
    })

    #### t-distribution Server Logic ####

    observe({
        t <- input$t
        df <- ifelse(input$df > 0, input$df, 1)
        tt <- input$t_tail
        ta <- input$t_arrow
        tu <- input$t_area

        output$t_header <- renderUI({
            h2("The ", em("t"), "-distribution")
        })


        #### dt_density ####
        # ggplot statistical function for shading area under Normal curve

        dt_density <- reactive({
            req(t)
            req(df)
            req(tu)
            function(x) {
                t_den <- dt(x, df = df)
                return(t_den)
            }
        })
        dt_tail <- reactive({
            req(t)
            req(df)
            req(tu)
            if (tt == "left") {
                function(x) {
                    t_left <- dt(x, df = df)
                    t_left[x <= -4 | x >= t] <- NA
                    return(t_left)
                }
            }
            else if (tt == "right") {
                function(x) {
                    t_right <- dt(x, df = df)
                    t_right[x <= t | x >= 4] <- NA
                    return(t_right)
                }
            }
            else if (tt == "middle") {
                function(x) {
                    t_mid <- dt(x, df = df)
                    t_mid[x <= -t | x >= t] <- NA
                    return(t_mid)
                }
            }
            else if (tt == "two") {
                function(x) {
                    t_two <- dt(x, df = df)
                    t_two[x >= -t & x <= t] <- NA
                    return(t_two)
                }
            }
        })

        #### t_area_fun ####
        # function to compute area under t curve from t-statistic
        t_area_fun <- reactive({
            req(df)
            req(t)
            req(tu)
            if (tt == "left") {
                round(pt(
                    q = t,
                    df = df,
                    lower.tail = TRUE
                ),
                digits = 5)
            }
            else if (tt == "right") {
                round(pt(
                    q = t,
                    df = df,
                    lower.tail = FALSE
                ),
                digits = 5)
            }
            else if (tt == "middle") {
                round(
                    pt(
                        q = t,
                        df = df,
                        lower.tail = TRUE
                    ) -
                        pt(
                            q = -t,
                            df = df,
                            lower.tail = TRUE
                        ),
                    digits = 5
                )
            }
            else if (tt == "two") {
                round(
                    pt(
                        q = t,
                        df = df,
                        lower.tail = FALSE
                    ) +
                        pt(
                            q = -t,
                            df = df,
                            lower.tail = TRUE
                        ),
                    digits = 5
                )
            }
        })

        #### t_fun ####
        # function to compute t-statistic from area under the curve
        t_fun <- reactive({
            req
            req(df)
            req(tu)
            if (tt == "left") {
                round(qt(
                    p = tu,
                    df = df,
                    lower.tail = TRUE
                ),
                digits = 5)
            }
            else if (tt == "right") {
                round(qt(
                    p = tu,
                    df = df,
                    lower.tail = FALSE
                ),
                digits = 5)
            }
            else if (tt == "middle") {
                round(qt(
                    p = (tu + ((
                        1 - tu
                    ) / 2)),
                    df = df,
                    lower.tail = TRUE
                ),
                digits = 5)
            }
            else if (tt == "two") {
                round(qt(
                    p = tu / 2,
                    df = df,
                    lower.tail = FALSE
                ),
                digits = 5)
            }
        })

        #### t_area_value ####
        t_area_value <- reactive({
            req(t)
            req(df)
            if (ta == "down") {
                c(round(t_area_fun(), 5))
            }
            else if (ta == "up") {
                if (tu > 0 & tu < 1) {
                    c(tu)
                }
                else if (tu == 0) {
                    c(0.01)
                }
                else if (tu == 1) {
                    c(0.99)
                }
            }
        })

        t_area_label <- reactive({
            req(t)
            req(tu)
            req(df)
            if (ta == "down") {
                c("Area Under the Curve")
            }
            else if (ta == "up") {
                c("Enter Area Under the Curve (0 to 1)")
            }
        })

        t_value <- reactive({
            req(tu)
            req(df)
            req(t)
            if (ta == "up") {
                if (tu > 0 & tu < 1) {
                    c(round(t_fun(), 5))
                }
                else {
                    c(1)
                }
            }
            else if (ta == "down") {
                if (tt == "left" | tt == "right") {
                    c(t)
                }
                else if (tt == "middle" | tt == "two") {
                    if (t >= 0) {
                        c(t)
                    }
                    else if (t < 0) {
                        c(-t)
                    }
                }
            }
        })

        t_label <- reactive({
            req(t)
            req(df)
            req(tu)
            if (ta == "up") {
                c("t-statistic")
            }
            else if (ta == "down") {
                if (tt == "left" | tt == "right") {
                    c("Enter t-statistic")
                }
                else if (tt == "middle" | tt == "two") {
                    c("Enter t-statistic (Positive)")
                }
            }
        })

        tu_min <- reactive({
            req(t)
            req(df)
            req(tu)
            if (ta == "down") {
                c(round(t_area_fun(), 5))
            }
            else if (ta == "up") {
                c(0.00001)
            }
        })

        tu_max <- reactive({
            req(t)
            req(df)
            req(tu)
            if (ta == "down") {
                c(round(t_area_fun(), 5))
            }
            else if (ta == "up") {
                c(0.99999)
            }
        })

        t_min <- reactive({
            req(tu)
            req(df)
            req(t)
            if (ta == "up") {
                c(round(t_fun(), 5))
            }
            else if (ta == "down") {
                if (tt == "left" | tt == "right") {
                    c(NA)
                }
                else if (tt == "middle" | tt == "two") {
                    c(0)
                }
            }
        })

        t_max <- reactive({
            req(t)
            req(tu)
            req(df)
            if (ta == "up") {
                c(round(t_fun(), 5))
            }
            else if (ta == "down") {
                c(NA)
            }
        })

        t_plot_area_label <- reactive({
            req(t)
            req(tu)
            req(df)
            if ((round(t_area_fun(), 4) * 100) < 0.01) {
                paste0("Area: ", c(formatC(
                    t_area_fun(),
                    format = "e",
                    digits = 3
                )))
            }
            else {
                paste0("Area: ",
                       round(t_area_fun(), 4) * 100, "%")
            }
        })

        t_plot_title <- reactive({
            req(df)
            req(t)
            req(tu)
            if (df == 1) {
                bquote(
                        italic("t") ~
                       "-distribution with" ~
                       .(df) ~
                       "degree of freedom"
                    )
                # )
                # expression(italic("t") ~ "-distribution with" ~ df ~ "Degree of Freedom")
            }
            else {
                bquote(
                        italic("t") ~
                       "-distribution with" ~
                       .(df) ~
                       "degrees of freedom"
                    )
            }
        })

        df_value <- reactive({
            req(df)
            req(t)
            req(tu)
            if (df == 0) {
                c(1)
            }
            else {
                c(df)
            }
        })

        updateNumericInput(
            session,
            "t",
            label = t_label(),
            value = t_value(),
            min = t_min(),
            max = t_max()
        )

        updateNumericInput(
            session,
            "t_area",
            label = t_area_label(),
            value = t_area_value(),
            min = tu_min(),
            max = tu_max()
        )

        updateNumericInput(session,
                           "df",
                           value = df_value())

        #### T PLOT ####
        output$tPlot <- renderPlot({
            ggplot(xvalues, aes(x = xvalues$x)) +
                stat_function(fun = dt_density(),
                              size = .9,
                              na.rm = TRUE) +
                stat_function(
                    fun = dt_tail(),
                    geom = "area",
                    fill = "orange",
                    alpha = 0.5
                ) +
                xlab(expression(italic("t") ~ "-statistic (t)")) +
                labs(# x = " \n t-statistic (t)",
                    y = "",
                    title = t_plot_title()) +
                geom_text(
                    x = 2.1,
                    y = 0.3,
                    size = 6,
                    fontface = "bold",
                    colour = "brown",
                    label = t_plot_area_label()
                ) +
                geom_text(
                    x = 2.1,
                    y = 0.25,
                    size = 6,
                    fontface = "bold",
                    colour = "brown",
                    label = paste0("t: ",
                                   formatC(
                                       round(t_fun(), 4),
                                       format = "f",
                                       digits = 2
                                   ))
                ) +
                theme(
                    plot.title = element_text(
                        face = "bold",
                        size = 16,
                        hjust = 0.5
                    ),
                    axis.title.x = element_text(
                        face = "bold",
                        colour = "brown",
                        size = 14
                    ),
                    axis.title.y = element_text(
                        face = "bold",
                        colour = "brown",
                        size = 12
                    ),
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_blank()
                ) +
                scale_x_continuous(limits = c(-4, 4),
                                   breaks = c(-4, -3, -2, -1, 0, 1, 2, 3, 4)) +
                scale_y_continuous(breaks = NULL)
        })

        #### uiOutput objects ####
        email <- a("xander@berkeley.edu",
                   href = "mailto:xander@berkeley.edu")

        school <- a("UC Berkeley School of Public Health",
                    href = "https://publichealth.berkeley.edu/")

        github <- a("Github.",
                    href = "https://github.com/posnerab/surfstat target=_blank",
                    rel = "noopener noreferrer",
                    target = "_blank",)

        output$contact <- renderUI({
            tagList(p("© 2019 Xander Posner, ", email),
                    p("MPH '20 | Epidemiology & Biostatistics"),
                    p(school))
        })

        output$code <- renderUI({
            tagList(p("This Shiny app was made in RStudio"),
                    p("Check out the source code on ", github))
        })

    }) # closes out top-level server observe({})
} # closes out server function{}

# Run the application
shinyApp(ui = ui, server = server)

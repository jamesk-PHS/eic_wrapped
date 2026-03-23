Home_Page <- list(
  card(
    full_screen = TRUE,
    card_header(
      "Welcome to CAIR Wrapped"
    ),
    card_body(
      markdown("This is an experimental dashboard from the Public Health Scotland Excellence in Care team.
               Each year the [CAIR Dashboard](https://eviz.seer.scot.nhs.uk/#/site/NSS/views/CAIRHomePage/HomePage?:iid=1) resets,
               so we thought it was a good time to reflect on the hard work and progress that colleagues have achieved.")
    )
  ),
  card(
    full_screen = TRUE,
    card_header("What's on the dashboard?"),
    card_body(
      markdown("This dashboard takes a completely differnet approach to showing you EiC data. 
      As on our main dashboard, we've split up how you can view data across Health Board, Location and 
               Team level submissions. Use the tabs above to navigate to the view you'd like to see.")
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Disclosure, Data Security Statement and Disclaimer"),
    card_body(
      markdown("In many cases the CAIR system contains information that has not been published 
  by NHS Boards and Health and Social Care Partnerships. Whilst there is no 
  personal information held within CAIR, the underlying data is not disclosure 
  controlled: visualisations may show information which could enable (perhaps 
  with the aid of additional knowledge or information) an individual patient or 
  member of staff to be identified. Security of the data during access and where
  reports have been downloaded and/or printed off remains the responsibility of
  each individual user and must be upheld in line with your local organisation’s 
  information governance policies. 
  \n
  Further information on the appropriate use of Care Assurance Improvement 
Resource (CAIR) Data is available [here](https://publichealthscotland.scot/our-areas-of-work/acute-and-emergency-services/excellence-in-care/cair-dashboard/appropriate-use-of-cair-data/).")
    )
  )
)

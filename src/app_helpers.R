
# functions to set appropriate background colors for date games table

expected_highlight <- function(ht) {
  
  set_background_color(
    ht = ht,
    row = where(
      ht$home_win_prob > 0.5 & ht$home_pts > ht$visitor_pts |
        ht$home_win_prob < 0.5 & ht$visitor_pts > ht$home_pts
    ),
    col = everywhere,
    value = jtc_blues[[3]],
    byrow = TRUE
  )
}

upset_highlight <- function(ht) {
  
  set_background_color(
    ht = ht,
    row = where(
      ht$home_win_prob > 0.5 & ht$visitor_pts > ht$home_pts |
        ht$home_win_prob < 0.5 & ht$home_pts > ht$visitor_pts
    ),
    col = everywhere,
    value = jtc_oranges[[3]],
    byrow = TRUE
  )
}

# functions to set bold for winning team

home_win_bold <- function(ht) {
  set_bold(
    ht = ht,
    row = where(ht$home_pts > ht$visitor_pts),
    col = starts_with("home"),
    value = TRUE
  )
}

vis_win_bold <- function(ht) {
  set_bold(
    ht = ht,
    row = where(ht$home_pts < ht$visitor_pts),
    col = starts_with("vis"),
    value = TRUE
  )
}
  


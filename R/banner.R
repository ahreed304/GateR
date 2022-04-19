library(crayon)
# library(xfun)
# library(hues)
# library(RColorBrewer)

#' Display Banner
#'
#' @return
#' @export
#'
#' @examples
#' display_banner()
display_banner <- function() {
  # foreground <- make_style("grey50")
  # background <- make_style("purple", bg = TRUE)
  # fancy <- combine_styles(foreground, background)
  #   r"{
  #            o  o  o                 o  o  o     o  o  o
  #         o  |  |  |  o           o  |  |  |  o  |  |  |  o
  #      o--o  o  |  |  |  o     o  |  |  |  o     o  |  |  |  o
  #    o------o   o  |  |  |  o  |  |  |  o           o  |  |  |  o
  #  o--------o      o  o  o     o  o  o                 o  o  o o--o
  # o--------o           _---~~(~~-_.            __             o-----o
  # o-------o          _{        )   )           ||            o--------o
  #  o----o          ,   ) -~~- ( ,-' )_        ====           o---------o
  #     o           (  `-,_..`., )-- '_,)       |  |__          o--------o
  #   o--o         ( ` _)  (  -~( -_ `,  }      |  |-.\           o-----o
  #  o-----o       (_-  _  ~_-~~~~`,  ,' )      |__|  \\              o
  # o--------o       `~ -^(    __;-,((()))       ||   ||            o--o
  # o---------o            ~~~~ {_ -_(())      ======__|          o------o
  #  o--------o                   `\  }       ________||__      o--------o
  #    o-----o                      { }      /____________\    o--------o
  #        o         ______      __          __          __    o-------o
  #      o--o       / ____/___ _/ /____     / /   ____ _/ /_    o----o
  #    o------o    / / __/ __ `/ __/ _ \   / /   / __ `/ __ \      o
  #  o--------o   / /_/ / /_/ / /_/  __/  / /___/ /_/ / /_/ /    o---o
  # o--------o    \____/\__,_/\__/\___/  /_____/\__,_/_.___/    o------o
  # o-------o                                                  o--------o
  #  o----o o  o  o                 o  o  o     o  o  o        o--------o
  #      o  |  |  |  o           o  |  |  |  o  |  |  |  o     o------o
  #         o  |  |  |  o     o  |  |  |  o     o  |  |  |  o   o--o
  #            o  |  |  |  o  |  |  |  o           o  |  |  |  o
  #               o  o  o     o  o  o                 o  o  o
  # }" |> fancy() |> cat()
  # }


  #   banner <- r"{
  #            o  o  o                 o  o  o     o  o  o                      |
  #         o  |  |  |  o           o  |  |  |  o  |  |  |  o                   |
  #      o--o  o  |  |  |  o     o  |  |  |  o     o  |  |  |  o                |
  #    o------o   o  |  |  |  o  |  |  |  o           o  |  |  |  o             |
  #  o--------o      o  o  o     o  o  o                 o  o  o o--o           |
  # o--------o           _---~~(~~-_.            __             o-----o         |
  # o-------o          _{        )   )           ||            o--------o       |
  #  o----o          ,   ) -~~- ( ,-' )_        ====           o---------o      |
  #     o           (  `-,_..`., )-- '_,)       |  |__          o--------o      |
  #   o--o         ( ` _)  (  -~( -_ `,  }      |  |-.\           o-----o       |
  #  o-----o       (_-  _  ~_-~~~~`,  ,' )      |__|  \\              o         |
  # o--------o       `~ -^(    __;-,((()))       ||   ||            o--o        |
  # o---------o            ~~~~ {_ -_(())      ======__|          o------o      |
  #  o--------o                   `\  }       ________||__      o--------o      |
  #    o-----o                      { }      /____________\    o--------o       |
  #        o         ______      __          __          __    o-------o        |
  #      o--o       / ____/___ _/ /____     / /   ____ _/ /_    o----o          |
  #    o------o    / / __/ __ `/ __/ _ \   / /   / __ `/ __ \      o            |
  #  o--------o   / /_/ / /_/ / /_/  __/  / /___/ /_/ / /_/ /    o---o          |
  # o--------o    \____/\__,_/\__/\___/  /_____/\__,_/_.___/    o------o        |
  # o-------o                                                  o--------o       |
  #  o----o o  o  o                 o  o  o     o  o  o        o--------o       |
  #      o  |  |  |  o           o  |  |  |  o  |  |  |  o     o------o         |
  #         o  |  |  |  o     o  |  |  |  o     o  |  |  |  o   o--o            |
  #            o  |  |  |  o  |  |  |  o           o  |  |  |  o                |
  #               o  o  o     o  o  o                 o  o  o                   |
  # }"





  # banner <- r"{
  # ________________________________________________________________________
  # |           o  o  o                 o  o  o     o  o  o                |
  # |        o  |  |  |  o           o  |  |  |  o  |  |  |  o             |
  # |     o--o  o  |  |  |  o     o  |  |  |  o     o  |  |  |  o          |
  # |   o------o   o  |  |  |  o  |  |  |  o           o  |  |  |  o       |
  # | o--------o      o  o  o     o  o  o                 o  o  o o--o     |
  # |o--------o           _---~~(~~-_.            __             o-----o   |
  # |o-------o          _{        )   )           ||            o--------o |
  # | o----o          ,   ) -~~- ( ,-' )_        ====           o---------o|
  # |    o           (  `-,_..`., )-- '_,)       |  |__          o--------o|
  # |  o--o         ( ` _)  (  -~( -_ `,  }      |  |-.\           o-----o |
  # | o-----o       (_-  _  ~_-~~~~`,  ,' )      |__|  \\              o   |
  # |o--------o       `~ -^(    __;-,((()))       ||   ||            o--o  |
  # |o---------o            ~~~~ {_ -_(())      ======__|          o------o|
  # | o--------o                   `\  }       ________||__      o--------o|
  # |   o-----o                      { }      /____________\    o--------o |
  # |       o         ______      __          __          __    o-------o  |
  # |     o--o       / ____/___ _/ /____     / /   ____ _/ /_    o----o    |
  # |   o------o    / / __/ __ `/ __/ _ \   / /   / __ `/ __ \      o      |
  # | o--------o   / /_/ / /_/ / /_/  __/  / /___/ /_/ / /_/ /    o---o    |
  # |o--------o    \____/\__,_/\__/\___/  /_____/\__,_/_.___/    o------o  |
  # |o-------o                                                  o--------o |
  # | o----o o  o  o                 o  o  o     o  o  o        o--------o |
  # |     o  |  |  |  o           o  |  |  |  o  |  |  |  o     o------o   |
  # |        o  |  |  |  o     o  |  |  |  o     o  |  |  |  o   o--o      |
  # |           o  |  |  |  o  |  |  |  o           o  |  |  |  o          |
  # |              o  o  o     o  o  o                 o  o  o             |
  # ------------------------------------------------------------------------
  # }"

    banner <- r"{
           o  o  o                 o  o  o     o  o  o
        o  |  |  |  o           o  |  |  |  o  |  |  |  o
     o--o  o  |  |  |  o     o  |  |  |  o     o  |  |  |  o
   o------o   o  |  |  |  o  |  |  |  o           o  |  |  |  o
 o--------o      o  o  o     o  o  o                 o  o  o o--o
o--------o           _---~~(~~-_.            __             o-----o
o-------o          _{        )   )           ||            o--------o
 o----o          ,   ) -~~- ( ,-' )_        ====           o---------o
    o           (  `-,_..`., )-- '_,)       |  |__          o--------o
  o--o         ( ` _)  (  -~( -_ `,  }      |  |-.\           o-----o
 o-----o       (_-  _  ~_-~~~~`,  ,' )      |__|  \\              o
o--------o       `~ -^(    __;-,((()))       ||   ||            o--o
o---------o            ~~~~ {_ -_(())      ======__|          o------o
 o--------o                   `\  }       ________||__      o--------o
   o-----o                      { }      /____________\    o--------o
       o         ______      __          __          __    o-------o
     o--o       / ____/___ _/ /____     / /   ____ _/ /_    o----o
   o------o    / / __/ __ `/ __/ _ \   / /   / __ `/ __ \      o
 o--------o   / /_/ / /_/ / /_/  __/  / /___/ /_/ / /_/ /    o---o
o--------o    \____/\__,_/\__/\___/  /_____/\__,_/_.___/    o------o
o-------o                                                  o--------o
 o----o o  o  o                 o  o  o     o  o  o        o--------o
     o  |  |  |  o           o  |  |  |  o  |  |  |  o     o------o
        o  |  |  |  o     o  |  |  |  o     o  |  |  |  o   o--o
           o  |  |  |  o  |  |  |  o           o  |  |  |  o
              o  o  o     o  o  o                 o  o  o
}"

  color_map <- r"{
           o  o  o                 o  o  o     o  o  o
        o  y  y  y  o           o  y  y  y  o  y  y  y  o
     oyyo  o  y  y  y  o     o  y  y  y  o     o  y  y  y  o
   oyyyyyyo   o  y  y  y  o  y  y  y  o           o  y  y  y  o
 oyyyyyyyyo      o  o  o     o  o  o                 o  o  o oyyo
oyyyyyyyyo           bbbbbbbbbbbb            ee             oyyyyyo
oyyyyyyyo          bb        b   b           ee            oyyyyyyyyo
 oyyyyo          b   b bbbb b bbb bb        eeee           oyyyyyyyyyo
    o           b  bbbbbbbbb bbb bbbb       e  eee          oyyyyyyyyo
  oyyo         b b bb  b  bbb bb bb  b      e  eeee           oyyyyyo
 oyyyyyo       bbb  b  bbbbbbbbb  bb b      eeee  ee              o
oyyyyyyyyo       bb bbb    bbbbbbbbbbb       ee   ee            oyyo
oyyyyyyyyyo            bbbb bb bbbbbb      eeeeeeeee          oyyyyyyo
 oyyyyyyyyo                   bb  b       eeeeeeeeeeee      oyyyyyyyyo
   oyyyyyo                      b b      eeeeeeeeeeeeee    oyyyyyyyyo
       o         dddddd      dd          dd          dd    oyyyyyyyo
     oyyo       d dddddddd dd ddddd     d d   dddd dd dd    oyyyyo
   oyyyyyyo    d d ddd dd dd ddd d d   d d   d dd dd dd d      o
 oyyyyyyyyo   d ddd d ddd d ddd  ddd  d ddddd ddd d ddd d    oyyyo
oyyyyyyyyo    ddddddddddddddddddddd  ddddddddddddddddddd    oyyyyyyo
oyyyyyyyo                                                  oyyyyyyyyo
 oyyyyo o  o  o                 o  o  o     o  o  o        oyyyyyyyyo
     o  y  y  y  o           o  y  y  y  o  y  y  y  o     oyyyyyyo
        o  y  y  y  o     o  y  y  y  o     o  y  y  y  o   oyyo
           o  y  y  y  o  y  y  y  o           o  y  y  y  o
              o  o  o     o  o  o                 o  o  o
}"

  # color_map <- r"{
  # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  # x           o  o  o                 o  o  o     o  o  o                x
  # x        c  y  y  y  o           o  y  y  y  o  y  y  y  o             x
  # x     aatt  o  y  y  y  o     o  y  y  y  o     o  y  y  y  o          x
  # x   ttttaaaa   o  y  y  y  o  y  y  y  o           o  y  y  y  o       x
  # x cccccggggg      o  o  o     o  o  o                 o  o  o oyyo     x
  # xgccccggggg           bbbbbbbbbbbb            ee             oyyyyyo   x
  # xggggccccc          bb        b   b           ee            oyyyyyyyyo x
  # x aaattt          b   b bbbb b bbb bb        eeee           oyyyyyyyyyox
  # x    t           b  bbbbbbbbb bbb bbbb       e  eee          oyyyyyyyyox
  # x  ggcc         b b bb  b  bbb bb bb  b      e  eeee           oyyyyyo x
  # x tttaaaa       bbb  b  bbbbbbbbb  bb b      eeee  ee              o   x
  # xtttttaaaaa       bb bbb    bbbbbbbbbbb       ee   ee            oyyo  x
  # xtttttaaaaaa            bbbb bb bbbbbb      eeeeeeeee          oyyyyyyox
  # x aaaaattttt                   bb  b       eeeeeeeeeeee      oyyyyyyyyox
  # x   cccgggg                      b b      eeeeeeeeeeeeee    oyyyyyyyyo x
  # x       a         dddddd      dd          dd          dd    oyyyyyyyo  x
  # x     ggcc       d dddddddd dd ddddd     d d   dddd dd dd    oyyyyo    x
  # x   ggggcccc    d d ddd dd dd ddd d d   d d   d dd dd dd d      o      x
  # x aaaaattttt   d ddd d ddd d ddd  ddd  d ddddd ddd d ddd d    oyyyo    x
  # xtttttaaaaa    ddddddddddddddddddddd  ddddddddddddddddddd    oyyyyyyo  x
  # xccccggggg                                                  oyyyyyyyyo x
  # x gggccc a  a  a                 o  o  o     o  o  o        oyyyyyyyyo x
  # x     g  t  a  a  g           o  y  y  y  o  y  y  y  o     oyyyyyyo   x
  # x        t  t  t  g  c     o  y  y  y  o     o  y  y  y  o   oyyo      x
  # x           t  t  c  g  c  y  y  y  o           o  y  y  y  o          x
  # x              t  c  g     o  o  o                 o  o  o             x
  # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  # }"
  # banner <- gsub('|\n', '\n', banner)
  # banner_split <- strsplit(banner, '\n') |> lapply(raw_string)
  banner_split <- strsplit(banner, '\n')[[1]]
  color_map_split <- strsplit(color_map, '\n')[[1]]

  y_dim <- banner_split |> length()
  x_dim <- banner_split |> lapply(nchar) |> as.integer() |> max()

  shorter_dim <- min(x_dim, y_dim)

  # palette <- banner_split |> unique() |> length() |> iwanthue(0, 360, 36, 180, 13, 73)


  # palette <- max(x_dim, y_dim) |> RPMG::rainbow.colors()
  x_palette <- x_dim |> RPMG::rainbow.colors()
  split_point <- x_palette |> length() %/% 2
  # x_palette_alt <- c(x_palette[split_point:length(x_palette)], x_palette[1:(split_point - 1)])
  x_palette_alt <- c(x_palette[(split_point + 1):length(x_palette)], x_palette[1:split_point])
  y_palette <- y_dim |> RPMG::rainbow.colors()
  # palette <- max(x_dim, y_dim) |> iwanthue(0, 360, 36, 180, 13, 73)
  banner_split |> seq_along() |> lapply(\(i) {
    # bg_color <- make_style(y_palette[[i]], bg = TRUE)
    chars <- banner_split[[i]] |> strsplit('') %>% .[[1]]
    color_map_chars <- color_map_split[[i]] |> strsplit('') %>% .[[1]]
    chars |> length()
    chars |> seq_along() |> lapply(\(j) {
      color <- crayon::make_style('black')
      if (chars[[j]] != ' ') {
        color_legend <- list(a = 'red', t = 'blue', c = 'yellow', g = 'green',
                             b = '#f99ada',d = '#f9751d', e = 'cyan', o = 'green', x = 'black', y = 'blue', z='black')
        color <- crayon::make_style(color_legend[[color_map_chars[[j]]]])
      }
      # if(color_map_chars[[j]] == 'b') {color <- 'pink'}
      # else {color <- 'blue'}
      # bg_color <- make_style(x_palette_alt[[j]], bg = TRUE)
      # rainbow <- combine_styles(color, bg_color)
      chars[[j]] |> color()
      # }) |> paste(collapse="") |> bg_color()
    }) |> paste(collapse="")
  }) |> paste('\n') |> cat()
}

#' Display Banner
#'
#' @param banner ascii banner
#' @param color_map color codes for banner
#' @param banner list of character-color pairs
#' @return
#' @export
#'
#' @examples
#' display_banner()
display_banner <- function(banner, color_map, color_legend) {
  color_legend <- color_legend |> lapply(crayon::make_style)
  banner_split <- strsplit(banner, '\n') |> unlist()
  color_map_split <- strsplit(color_map, '\n') |> unlist()
  banner_split |> seq_along() |> lapply(\(i) {
    banner_chars <- banner_split[[i]] |> strsplit('') |> unlist()
    color_map_chars <- color_map_split[[i]] |> strsplit('') |> unlist()
    banner_chars |> seq_along() |> lapply(\(j) {
      color <- color_legend$default
      if (color_map_chars[[j]] %in% names(color_legend)) { color <- color_legend[[color_map_chars[[j]]]] }
      banner_chars[[j]] |> color()
    }) |> paste(collapse="")
  }) |> paste('\n') |> cat()
}


#' Gate Lab Banner
#'
#' @return
#' @export
#'
#' @examples
#' gatelab_banner()
gatelab_banner <- function() {
  display_banner(
    banner = r"{
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
}",
    color_map = r"{
           o  o  o                 x  x  x     o  o  o
        x  y  y  y  o           x  y  y  y  x  y  y  y  o
     xyyo  x  y  y  y  o     x  y  y  y  o     x  y  y  y  o
   xyyyyyyo   x  y  y  y  o  y  y  y  o           x  y  y  y  o
 xyyyyyyyyo      x  x  x     o  o  o                 x  x  x oyyx
xyyyyyyyyo           bbbbbbbbbbbb            ee             oyyyyyx
xyyyyyyyo          bb        b   b           ee            oyyyyyyyyx
 xyyyyo          b   b bbbb b bbb bb        eeee           oyyyyyyyyyx
    o           b  bbbbbbbbb bbb bbbb       e  eee          oyyyyyyyyx
  oyyx         b b bb  b  bbb bb bb  b      e  eeee           oyyyyyx
 oyyyyyx       bbb  b  bbbbbbbbb  bb b      eeee  ee              x
oyyyyyyyyx       bb bbb    bbbbbbbbbbb       ee   ee            xyyo
oyyyyyyyyyx            bbbb bb bbbbbb      eeeeeeeee          xyyyyyyo
 oyyyyyyyyx                   bb  b       eeeeeeeeeeee      xyyyyyyyyo
   oyyyyyx                      b b      eeeeeeeeeeeeee    xyyyyyyyyo
       x         dddddd      dd          dd          dd    xyyyyyyyo
     xyyo       d dddddddd dd ddddd     d d   dddd dd dd    xyyyyo
   xyyyyyyo    d d ddd dd dd ddd d d   d d   d dd dd dd d      o
 xyyyyyyyyo   d ddd d ddd d ddd  ddd  d ddddd ddd d ddd d    oyyyx
xyyyyyyyyo    ddddddddddddddddddddd  ddddddddddddddddddd    oyyyyyyx
xyyyyyyyo                                                  oyyyyyyyyx
 xyyyyo x  x  x                 o  o  o     x  x  x        oyyyyyyyyx
     o  y  y  y  x           o  y  y  y  o  y  y  y  x     oyyyyyyx
        o  y  y  y  x     o  y  y  y  x     o  y  y  y  x   oyyx
           o  y  y  y  x  y  y  y  x           o  y  y  y  x
              o  o  o     x  x  x                 o  o  o
}",
    color_legend = list(
      a = 'red', t = 'blue', c = 'yellow', g = 'green', b = '#f99ada', d = '#f9751d', 
      e = 'cyan', o = 'green',x = 'purple', y = 'blue', z='black', default = 'black'
    )
  )
}


#' GateR Banner
#'
#' @return
#' @export
#'
#' @examples
#' gater_banner()
gater_banner <- function() {
  display_banner(
    banner = r"{
           .-._   _ _ _ _ _ _ _ _
.-''-.__.-'00  '-' ' ' ' ' ' ' ' '-._
'.___ '    .   .--_'-' '-' '-' _'-'  '-. 
 V: V 'vv-'   '_   '.       .'  _.._     '.
   '=.____.=_.--'   :_.__.__:_   '.  '-.   '
           (((____.-'        '-.  /     \   \
  ________        __        _(((-'____   '   ' 
 /  _____/_____ _/  |_  ____\______   \   '  |
/   \  ___\__  \\   __\/ __ \|       _/   '  ' 
\    \_\  \/ __ \|  | \  ___/|    |   \   | /
 \______  (____  /__|  \___  >____|_  /   |/
        \/     \/          \/       \/
}",
    color_map = r"{
           gggg   g g g g g g g g
gggggggggggww  ggg g g g g g g g gggg
ggggg g    g   ggggggg ggg ggg gggg  ggg 
 wg w gwwgg   gg   gg       gg  gggg     gg
   gggggggggggggg   gggggggggg   gg  ggg   g
           aaaggggggg        ggg  g     g   g
  bbbbbbbb        bb        baaaggbbbb   g   g 
 b  bbbbbbbbbbb bb  bb  bbbbbbbbbbb   b   g  g
b   b  bbbbbb  bb   bbbb bb bb       bb   g  g 
b    bbb  bb bb bb  b b  bbbbb    b   b   g g
 bbbbbbb  bbbbb  bbbb  bbbb  bbbbbbb  b   gg
        bb     bb          bb       bb
}",
    color_legend = list(g = 'green', w = 'white', a = 'green', b = '#f9751d', default = 'black')
  )
}




#' Austino Banner
#'
#' @return
#' @export
#'
#' @examples
#' austino_banner()
austino_banner <- function() {
  display_banner(
    banner = r"{
   / \    |   |   |    / \     -------   -------  |\   |  |    / \
  /   \   |   |   |   /   \   |       | |       | | \  |  |   /   \
 /     \  |   |   |  /     \  |  | |  | |  | |  | |  \ |  |  /     \
|   |   | |   |   | |   |   |    | |       | |    |   \|  | |   |   |
|   |   | |   |   | |   |   |    | |       | |    |    \  | |   |   |
|       | |   |   |  \   \       | |       | |    |       | |   |   |
|       | |   |   |    \   \     | |       | |    |       | |   |   |
|   |   | |   |   | |   |   |    | |       | |    |  \    | |   |   |
|   |   | |   |   | |   |   |    | |       | |    |  |\   | |   |   |
|   |   | |       |  \     /     | |    |  | |  | |  | \  |  \     /
|   |   |  \     /    \   /      | |    |       | |  |  \ |   \   /
|   |   |   \___/      \ /       | |     -------  |  |   \|    \ /
}",
    color_map = r"{
   / \    |   |   |    / \     -------   -------  |\   |  |    / \
  /   \   |   |   |   /   \   |       | |       | | \  |  |   /   \
 /     \  |   |   |  /     \  |  | |  | |  | |  | |  \ |  |  /     \
|   |   | |   |   | |   |   |    | |       | |    |   \|  | |   |   |
|   |   | |   |   | |   |   |    | |       | |    |    \  | |   |   |
|       | |   |   |  \   \       | |       | |    |       | |   |   |
|       | |   |   |    \   \     | |       | |    |       | |   |   |
|   |   | |   |   | |   |   |    | |       | |    |  \    | |   |   |
|   |   | |   |   | |   |   |    | |       | |    |  |\   | |   |   |
|   |   | |       |  \     /     | |    |  | |  | |  | \  |  \     /
|   |   |  \     /    \   /      | |    |       | |  |  \ |   \   /
|   |   |   \___/      \ /       | |     -------  |  |   \|    \ /
}",
    color_legend = list('\\' = 'green', '/' = 'blue', '|' = 'magenta', '-' = 'yellow', '_' = 'yellow', default = 'purple')
  )
}
austino_banner()
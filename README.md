* MusicScroll

![Basic program for basic needs](https://i.imgur.com/BEa6RzJ.png)

I don't like having to fire up a browser to get the lyrics of the songs
I am currently hearing. I got basic needs, I download songs with good
metadata and use SMPlayer to open the playlists. It would be nice if
somehow I could get a basic windows with the lyrics of the song.

MusicScroll does this. When you run the command `music-scroll` a window
is opened with the lyrics of the song your are playing on SMPlayer and
each change of song will retrieve new lyrics without your intervention.

This works by getting the MPRIS info on your song, specifically the
title and artists fields. After some clean up it will retrieve the lyrics
from a lyrics website, clean the HTML a little and put it as text on a
GTK window. Ideally it ought be not invasive so you can read only if you
want.

This was done as an excuse to learn more of gi-gtk & glade. But I am
happy with the result so I am sharing. On the future I would like do
write the lyrics to a SQLite database for offline usage and handle other
videoplayers as VLC (this ought to be really easy, send a patch).

I want to thanks to the `haskell-gi` and `haskell dbus` authors. I
didn't know much about graphics programming or dbus, but the
documentation, types and code samples helped me a lot on understanding
how they ought to be organized. If I see you guys on a conference I will
buy you a beer :-) . The `d-feet` program was really helpful for see how
the data was layout on DBus.

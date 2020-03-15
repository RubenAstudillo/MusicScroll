# MusicScroll

![Basic program for basic needs](https://i.imgur.com/Jk5kmYQ.png)

I don't like having to fire up a browser to get the lyrics of the songs
I am currently hearing. I got basic needs, I download songs with good
metadata and use SMPlayer/VLC to open the playlists. It would be nice if
somehow I could get a basic windows with the lyrics of the song.

MusicScroll does this. When you run the command `music-scroll`, a window
is opened with the lyrics of the song your are playing on SMPlayer/VLC
and each change of song will retrieve new lyrics hopefully without your
intervention. If it could not retrieve the song because it was lacking
metadata, you could supply the missing info to get it. It will cache the
results so you don't have to intervene manually next time. Ideally it
ought be not invasive so you can read only if you want.

This works by getting the MPRIS info on your song, specifically the
title and artists fields. After some clean up it will retrieve the
lyrics from a lyrics website, clean the HTML a little and put it as text
on a GTK window. It will cache the results on a SQLite DB for next time.

This was done as an excuse to learn more of gi-gtk, glade and SQLite.
But I am happy with the result so I am sharing.

I want to thanks to the `haskell-gi` and `haskell dbus` authors. I
didn't know much about UI programming or dbus, but the documentation,
types and code samples helped me a lot on understanding how they ought
to be organized. If I see you guys on a conference I will buy you a beer
:-) . The `d-feet` program was really helpful for see how the data was
layout on DBus.

This probably doesn't work, use at your own risk, etc.

basic usage: `(lytro-dl::download-pictures)`

By default, copies files to `~/lytro/<serial>/sha1-.../<*>` where
`serial` is the camera serial number, `sha1-...` is the sha1 ID of the
file from the file listing in the camera protocol. `~/lytro/` can be
changed by specifying `:DIR` option to `DOWNLOAD-PICTURES`.

Each directory contains a set of files like

```
100-0004.128  100-0004.jpg  100-0004.raw  100-0004.stk  100-0004.txt  metadata
```

`metadata` is a s-expression containing some metatdata from the camera. Most useful fields are probably `:rotation`, `:picture-date`, and `:last-lambda`.

Other files are directly as sent by camera, `###-####.txt` is json
metadata containing a bunch of camera and picture
settings. `###-####.raw` is the raw image data, with format specified
in json file (something like 3280x3280 big-endian 12bit/pixel (actual
values from 168-4095), with "r,gr:gb,b" bayer filter. Lenslet array
orientation/offset is also specified in the json data. `###-####.jpg`
is a jpeg compressed version of the raw, which loses most of the
lightfield data due to compression but can provide a quick
preview. `###-####.128` is thumbnail as 128x128 4:2:2
yuy2. `###-###.stk` is a binary file containing a few jpg files with
320x320 images at various focus settings. See
http://optics.miloush.net/lytro/TheProtocols.Commands.aspx under "load
picture" for details.

If it starts getting PIPE or TIMEOUT errors, might help to run
`(lytro-dl::%reset-camera)` to clear/halt/reset the USB endpoint.  It
if gets permission errors, may need to add
`/etc/udev/rules.d/51-lytro.rules` containing

```
SUBSYSTEM=="usb", ATTR{idVendor}=="24cf", ATTR{idProduct}=="00a1", MODE="0777"
```

to tell udev to make the device readable by default, then disconnect/reconnect the usb.

# MVCtranscoder

MVCtranscoder can be used to combine two elementry video streams to create a 3D MVC output.
MVCtranscoder can also transcode 3D/2D h264 streams with the aim of reducing the bitrate.

By default Intel Quick Sync hardware accelerated encoding is used.
Software only decoding/encoding options are offered for systems without Intel Quicksync support.

## The advantages of MVCtranscoder
1. Combine two two elementry video streams to create a 3D MVC output.
2. Provides a GUI for simple operation.
3. Allows for multiple simultaneous transcodes.
4. Uses a full decode/encode implimentation. The Intel transcode functionality sometimes produces MVC transcodes with missing frames.
5. Uses software decode to work around rare Intel MVC decode errors.
6. Allows for software only transcoding on systems without Intel QuickSync.
7. Free and opensource. Source code is avaliable under GPL for both the GUI frontend and the MVCtranscode backend.
8. Software transcoding supported on LINUX with Wine.

## MVC H264 Encoding Performance (balanced encoding speed)
```
        Hardware on I7-7700 ~76fps
        Software on I7-7700 ~12fps (OS Linux with Wine)
        Software on AMD FX-6300 ~6fps (OS Linux with Wine)
```

        This application works best with 4th generation Intel Core processor(codename Haswell) onward.
        Although not guaranteed, default settings are aimed at bluray compliance.
        These defaults are also geared towards high quality at a reasonable size.


![screenshot1](https://user-images.githubusercontent.com/1158312/33961955-c5e10b8c-e047-11e7-805c-8a81492b96fe.png)

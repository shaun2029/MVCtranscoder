# MVCtranscoder

MVCtranscoder can be used to transcode 3D and 2D h264 streams with the aim of reducing the bitrate.
By default Intel Quick Sync hardware accelerated encoding is used.
Software only decoding/encoding options are offered for systems without Intel Quicksync support.

        This application works best with 4th generation Intel Core processor(codename Haswell) onward.
        Although not guaranteed, default settings are aimed at bluray compliance.
        These defaults are also geared towards high quality at a reasonable size.

## MVC H264 Encoding Performance (balanced encoding speed)
```
        Hardware on I7-7700 ~76fps
        Software on I7-7700 ~12fps (OS Linux with WINE)
        Software on AMD FX-6300 ~6fps (OS Linux with WINE)
```

![screenshot1](https://user-images.githubusercontent.com/1158312/33799904-ecf2f0be-dd2c-11e7-9f95-d81841f482aa.png)

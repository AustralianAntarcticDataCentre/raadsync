raadsync
========

## Installing

```{r}
install.packages("devtools")
library(devtools)
install_github("AustralianAntarcticDataCentre/raadsync")
```

## Configuration files

Configuration files are used to tell `raadsync` which data sets to synchronise, along with settings such as the location of the data directory. A default configuration file is provided as part of the package:

```{r}
system.file("extdata","raad_repo_config.json",package="raadsync")
```

You will need to modify this configuration to suit your purposes (in particular, the default configuration sets `do_sync` to FALSE for all datasets, so it won't actually synchronise any data until you override one or more of these).

There are two different ways to manage configurations:

1. Use the default configuration with local adjustments (recommended if you intend to mostly use datasets already defined in the default file). Create a local configuration file that alters or adds to the default configuration. Start with the example local configuration file provided in the package:

  ```{r}
  file.copy(system.file("extdata","sample_local_config.json",package="raadsync"),"/path/to/your/local_config.json")
  ```

  Edit `/path/to/your/local_config.json` to suit your needs.

  Then to use this configuration in combination with the default configuration file:

  ```{r}
  cf=read_repo_config("/path/to/your/local_config.json")
  sync_repo(cf)
  ```


2. Use your own configuration file entirely. You can use the default configuration file as a starting point.

  When using solely your own configuration file, specify NULL for the `default_config_file` parameter in `read_repo_config`:

  ```{r}
  cf=read_repo_config("/path/to/your/config.json",NULL)
  sync_repo(cf)
  ```

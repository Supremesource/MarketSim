# Markert simulation software

prototype of a market simulation with transparent data


    Note : 
    - X = NEW LONG
    - Y = NEW SHORT
    - F = CLOSING LONG
    - Z = CLOSING SHORT



# User manual
Do not forget to specify runSettings in /Settings 


# References

- [POSSIBLE SETTINGS](/doc/SETTING.MD)

- [VOLUME PROOF](/doc/setThoryproof.pdf)

- [TODO](doc/TODO.md)


## Directory look

```bash

├── App
│   ├── Filepaths.hs
│   ├── Main
│   ├── Main.hs
│   └── tempCodeRunnerFile.hs
├── CHANGELOG.md
├── LICENSE
├── README.md
├── Settings
│   └── RunSettings.hs
├── Setup.hs
├── Src
│   ├── Colours.hs
│   ├── InputOutput.hs
│   ├── Lib.hs
│   ├── Statistics.hs
│   ├── Util.hs
│   └── tempCodeRunnerFile.hs
├── Types
│   └── DataTypes.hs
├── backtestapp
├── data
│   ├── askbook.txt
│   └── bidbook.txt
├── doc
│   ├── SETTING.MD
│   ├── TODO.md
│   ├── contents.txt
│   ├── observtions.md
│   └── set thory proof.pdf
├── hie.yaml
├── marketsim.cabal
├── output
│   ├── bidAskR.txt
│   ├── bidToAskR.txt
│   ├── buyVol.txt
│   ├── eLongs.txt
│   ├── eShorts.txt
│   ├── log.txt
│   ├── nLongs.txt
│   ├── nShorts.txt
│   ├── oInterest.txt
│   ├── pricehistory.txt
│   ├── sellVol.txt
│   └── volume.txt
├── package.yaml
├── scripts
│   └── plot_prices.py
├── stack.yaml
└── stack.yaml.lock

```


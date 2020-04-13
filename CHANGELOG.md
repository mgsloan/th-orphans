### 0.13.10 [2020.04.13]
* Implement `liftTyped` in the `Lift Bytes` instance.

### 0.13.9 [2019.09.28]
* Implement `qReifyType` (introduced in `template-haskell-2.16.0.0`) for the
  `Quasi` instances defined in `th-orphans`.

### 0.13.8 [2019.09.04]
* Backport the `Bounded` instance for `Extension`
  (from `Language.Haskell.TH.LanguageExtensions`), which was introduced in
  `template-haskell-2.15.0.0`.

### 0.13.7 [2019.03.24]
* Backport the `MonadFail Q` instance.
* Allow building with `template-haskell-2.16` by manually implementing
  `Lift` for `Bytes`. See [#27]

[#27]: https://github.com/mgsloan/th-orphans/issues/27

### 0.13.6 [2018.07.01]
* Allow building with `template-haskell-2.14`.
* Implement `qAddForeignFilePath` and `qAddTempFile` for `Quasi` instances

### 0.13.5 [2018.01.18]
* Implement `qAddCorePlugin` for `Quasi` instances

### 0.13.4 [2017.07.26]
* Implement `qAddForeignFile` for `Quasi` instances

### 0.13.3 [2016.11.09]
* Backport `Generic NameFlavour` instance

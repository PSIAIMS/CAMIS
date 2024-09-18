class SampleDesign:
    pass


# @frozen
# class AuxVars:
#     # TODO: Add missing values functionality
#     x: dict
#     nrecords: dict | Number
#     record_id: dict | None
#     domains: list | None
#     uid: int = int(dt.datetime.now(tz=dt.timezone.utc).strftime("%Y%m%d%H%M%S") + str(int(1e16 * rand.random())))

#     def __init__(
#         self,
#         x: DF | Array | Iterable[DF | Array] | None = None,
#         domain: Array | None = None,
#         record_id: Array = None,
#         **kwargs,
#     ) -> None:
#         assert x is not None or kwargs != {}

#         if isinstance(x, (DF, Array)):
#             aux_df = self.__from_df(x)
#             if isinstance(x, Array):
#                 aux_df.columns = ["__x_" + str(i) for i in range(aux_df.shape[1])]
#         elif isinstance(x, Iterable):
#             for i, d in enumerate(x):
#                 assert isinstance(d, (DF, Array))
#                 if i == 0:
#                     aux_df = self.__from_df(d)
#                     if isinstance(d, Array):
#                         aux_df.columns = ["__x_" + str(i) for i in range(aux_df.shape[1])]
#                 else:
#                     d_df = self.__from_df(d)
#                     if isinstance(d, Array):
#                         d_df.columns = ["__x_" + str(i) for i in range(d_df.shape[1])]
#                     aux_df.hstack([d_df], in_place=True)
#         else:
#             aux_df = None

#         if aux_df is None:
#             x = pl.from_dict(kwargs)
#         else:
#             x = aux_df.hstack(pl.from_dict(kwargs))

#         __record_id = (
#             numpy_array(record_id) if record_id is not None else np.linspace(0, x.shape[0] - 1, x.shape[0]).astype(int)
#         )

#         __domains = None
#         if domain is not None:
#             __domain = numpy_array(domain).tolist()
#             auxdata_dict = x.insert_column(0, pl.Series(__domain).alias("__domain")).partition_by(
#                 "__domain", as_dict=True
#             )

#             record_id_dict = pl.DataFrame([__domain, __record_id], schema=["__domain", "__record_id"]).partition_by(
#                 "__domain", as_dict=True
#             )

#             nrecords = {}
#             __domains = np.unique(__domain).tolist()
#             for d in __domains:
#                 auxdata_dict[d] = auxdata_dict[d].drop("__domain")
#                 record_id_dict[d] = record_id_dict[d]["__record_id"].to_list()
#                 nrecords[d] = auxdata_dict[d].shape[0]
#             auxdata_dict = {k: auxdata_dict[k].to_dict(as_series=False) for k in auxdata_dict}
#         else:
#             nrecords = x.shape[0]
#             auxdata_dict = x.to_dict()
#             record_id_dict = pl.DataFrame([__record_id], schema=["__record_id"]).to_dict()

#         # record_id_dict = {
#         #     k: record_id_dict[k].to_dict(as_series=False) for k in record_id_dict
#         # }

#         self.__attrs_init__(auxdata_dict, nrecords, record_id_dict, __domains)

#     def __from_df(self, auxdata: DF | Array) -> pl.DataFrame | None:
#         if isinstance(auxdata, pl.DataFrame):
#             return auxdata
#         elif isinstance(auxdata, pl.Series):
#             return pl.DataFrame(auxdata)
#         elif isinstance(auxdata, (pd.DataFrame, pd.Series)):
#             return pl.from_pandas(auxdata)
#         elif isinstance(auxdata, np.ndarray):
#             return pl.from_numpy(auxdata)
#         elif isinstance(auxdata, Array):
#             return pl.DataFrame(auxdata)
#         else:
#             return None

#     def to_numpy(
#         self,
#         keep_vars: str | Iterable[str] | None = None,
#         drop_vars: str | Iterable[str] | None = None,
#     ):
#         return self.to_polars(keep_vars=keep_vars, drop_vars=drop_vars).to_numpy()

#     def to_polars(
#         self,
#         keep_vars: str | Iterable[str] | None = None,
#         drop_vars: str | Iterable[str] | None = None,
#     ):
#         if self.domains is None:
#             auxdata = pl.from_dict(self.x).insert_at_idx(0, pl.Series(self.record_id).alias("__record_id"))
#         else:
#             auxdata = pl.concat(
#                 [
#                     pl.from_dict(self.x[d])
#                     .insert_at_idx(0, pl.Series(self.record_id[d]).alias("__record_id"))
#                     .insert_at_idx(
#                         1,
#                         pl.repeat(d, n=self.nrecords[d], eager=True).alias("__domain"),
#                     )
#                     for d in self.domains
#                 ]
#             )

#         if keep_vars is None and drop_vars is None:
#             varlist = auxdata.columns
#         elif keep_vars is not None:
#             varlist = keep_vars
#         elif drop_vars is not None:
#             varlist = [item for item in auxdata.columns if item not in drop_vars]
#         else:
#             raise TypeError("Keep_vars and drop_vars must be None or str or list[str]")

#         return auxdata.select(varlist)

#     def to_pandas(
#         self,
#         keep_vars: str | Iterable[str] | None = None,
#         drop_vars: str | Iterable[str] | None = None,
#     ):
#         return self.to_polars(keep_vars=keep_vars, drop_vars=drop_vars).to_pandas()


# class CovMat:
#     pass


# def _is_all_items_positive(obj: Array | DictStrNum) -> bool:
#     assert isinstance(obj, (np.ndarray, pd.Series, list, tuple, dict))
#     if isinstance(obj, dict):
#         arr = numpy_array(list(obj.values()))
#     else:
#         arr = numpy_array(obj).flatten()

#     if (arr <= 0).any():
#         return False
#     else:
#         return True

#     return (arr > 0).all()


# @frozen
# class DirectEst:
#     est: dict = field(validator=validators.instance_of(dict))
#     stderr: dict = field(validator=validators.instance_of(dict))
#     ssize: dict | None = None
#     psize: dict = None
#     domains: list | None = None
#     uid: int = int(dt.datetime.now(tz=dt.timezone.utc).strftime("%Y%m%d%H%M%S") + str(int(1e16 * rand.random())))

#     def __init__(
#         self,
#         est: dict | Array | Number,
#         stderr: dict | Array | Number,
#         ssize: dict | Array | Number | None = None,
#         psize: dict | Array | Number | None = None,
#         domain: Array | None = None,
#     ) -> None:
#         domains = None
#         if domain is not None:
#             domain = numpy_array(domain)
#             domains = domain.tolist()

#             if isinstance(stderr, Number):
#                 stderr = dict(zip(domain, tuple(np.repeat(stderr, len(domain)))))
#             if isinstance(stderr, Array):
#                 stderr = dict(zip(domain, stderr))
#             if isinstance(est, Number):
#                 est = dict(zip(domain, tuple(np.repeat(est, len(domain)))))
#             if isinstance(est, Array):
#                 est = dict(zip(domain, est))
#             if isinstance(ssize, Number):
#                 ssize = dict(zip(domain, tuple(np.repeat(ssize, len(domain)))))
#             if isinstance(ssize, Array):
#                 ssize = dict(zip(domain, ssize))

#         assert _is_all_items_positive(stderr)

#         if psize is not None:
#             if isinstance(ssize, Number):
#                 psize = dict(zip(domain, tuple(np.repeat(ssize, len(domain)))))
#             if isinstance(ssize, Array):
#                 psize = dict(zip(domain, stderr))

#         self.__attrs_init__(est, stderr, ssize, psize, domains)

#     @property
#     def cv(self):
#         return {
#             key: self.stderr[key] / self.est[key] if self.est[key] != 0 else float("inf") * self.stderr[key]
#             for key in self.stderr
#         }

#     def to_numpy(
#         self,
#         keep_vars: str | Iterable[str] | None = None,
#         drop_vars: str | Iterable[str] | None = None,
#     ):
#         return self.to_polars(keep_vars=keep_vars, drop_vars=drop_vars).to_numpy()

#     def to_polars(
#         self,
#         keep_vars: str | Iterable[str] | None = None,
#         drop_vars: str | Iterable[str] | None = None,
#     ):
#         if self.domains is None:
#             aux_df = pl.from_dict(
#                 {
#                     "est": list(self.est.values()),
#                     "stderr": list(self.stderr.values()),
#                     "ssize": list(self.ssize.values()),
#                 }
#             )
#         else:
#             aux_df = pl.from_dict(
#                 {
#                     "__domain": list(self.est.keys()),
#                     "est": list(self.est.values()),
#                     "stderr": list(self.stderr.values()),
#                     "ssize": list(self.ssize.values()),
#                 }
#             )

#         if keep_vars is None and drop_vars is None:
#             varlist = aux_df.columns
#         elif keep_vars is not None:
#             varlist = keep_vars
#         elif drop_vars is not None:
#             varlist = [item for item in aux_df.columns if item not in drop_vars]
#         else:
#             raise TypeError("Keep_vars and drop_vars must be None or str or list[str]")

#         return aux_df.select(varlist)

#     def to_pandas(
#         self,
#         keep_vars: str | Iterable[str] | None = None,
#         drop_vars: str | Iterable[str] | None = None,
#     ):
#         return self.to_polars(keep_vars=keep_vars, drop_vars=drop_vars).to_pandas()


# @frozen
# class FitStats:  # MAYBE call this ModelStats or FitStats or ...
#     method: FitMethod
#     err_stderr: dict
#     fe_est: namedtuple  # fixed effects
#     fe_stderr: namedtuple
#     re_stderr: float
#     re_stderr_var: float
#     log_llike: float
#     convergence: dict
#     goodness: dict
#     uid: int = int(dt.datetime.now(tz=dt.timezone.utc).strftime("%Y%m%d%H%M%S")) + int(1e16 * rand.random())


# @frozen
# class EblupFit:  # MAYBE call this ModelStats or FitStats or ...
#     method: FitMethod
#     err_stderr: dict
#     fe_est: namedtuple  # fixed effects
#     fe_stderr: namedtuple
#     re_stderr: float
#     re_stderr_var: float
#     log_llike: float
#     convergence: dict
#     goodness: dict
#     uid: int = int(dt.datetime.now(tz=dt.timezone.utc).strftime("%Y%m%d%H%M%S")) + int(1e16 * rand.random())


# @frozen
# class EbFit:
#     method: FitMethod
#     err_stderr: float
#     fe_est: namedtuple  # fixed effects
#     fe_stderr: namedtuple
#     re_stderr: float
#     re_stderr_var: float
#     log_llike: float
#     convergence: dict
#     goodness: dict
#     uid: int = int(dt.datetime.now(tz=dt.timezone.utc).strftime("%Y%m%d%H%M%S")) + int(1e16 * rand.random())


# @frozen
# class EblupEst:
#     pred: dict
#     domains: list
#     fit_stats: EblupFit
#     mse: dict | None = None
#     mse_boot: dict | None = None
#     mse_jkn: dict | None = None
#     uid: int = int(dt.datetime.now(tz=dt.timezone.utc).strftime("%Y%m%d%H%M%S")) + int(1e16 * rand.random())

#     def __init__(
#         self,
#         pred: dict | Array,
#         fit_stats: EblupFit,
#         domain: Array | None = None,
#         mse: dict | Array | None = None,
#         mse_boot: dict | Array | None = None,
#         mse_jkn: dict | Array | None = None,
#     ) -> None:
#         if domain is not None:
#             domain = numpy_array(domain)
#             domains = domain.tolist()

#             if isinstance(pred, Array):
#                 pred = dict(zip(domain, pred))
#             if isinstance(mse, Array):
#                 dict(zip(domain, mse))
#             if isinstance(mse_boot, Array):
#                 dict(zip(domain, mse_boot))
#             if isinstance(mse_jkn, Array):
#                 dict(zip(domain, mse_jkn))
#         else:
#             domains = list(pred.keys())

#         self.__attrs_init__(pred, domains, fit_stats, mse, mse_boot, mse_jkn)

#     @property
#     def rse():
#         pass

#     @property
#     def cv():
#         pass

#     def to_numpy():
#         pass

#     def to_polars():
#         pass

#     def to_pandas():
#         pass


# @frozen
# class EbEst:
#     area: list
#     pred: dict
#     fit_stats: EbFit
#     mse: dict | None = None
#     mse_boot: dict | None = None
#     mse_jkn: dict | None = None
#     uid: int = int(dt.datetime.now(tz=dt.timezone.utc).strftime("%Y%m%d%H%M%S")) + int(1e16 * rand.random())

#     @property
#     def rse():
#         pass

#     @property
#     def cv():
#         pass

#     def to_numpy():
#         pass

#     def to_polars():
#         pass

#     def to_pandas():
#         pass

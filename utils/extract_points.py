import xarray as xr


# fname = "www/data/ncs/cmip6/pr/hist/pr_hist_month-10_19610101-20141231.nc"
# lon = 25
# lat = 46
# variable = 'pr'
def extract_point(fname, lon, lat, variable):
  ds = xr.open_dataset(fname)
  ds.close()
  dsloc = ds.sel(lon=lon,lat=lat,method='nearest')
  data = dsloc[variable].to_pandas().round(1)
  return(data)

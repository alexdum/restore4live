import xarray as xr


def extract_point(fname, lon, lat, variable):
    # Open the dataset
    ds = xr.open_dataset(fname)
    
    # Check if the dataset uses 'latitude'/'longitude' or 'lat'/'lon'
    if 'latitude' in ds and 'longitude' in ds:
        lat_name = 'latitude'
        lon_name = 'longitude'
    elif 'lat' in ds and 'lon' in ds:
        lat_name = 'lat'
        lon_name = 'lon'
    else:
        raise ValueError("Dataset must contain 'latitude'/'longitude' or 'lat'/'lon'")
    
    # Get the latitude and longitude ranges
    lon_min = ds[lon_name].min().item()
    lon_max = ds[lon_name].max().item()
    lat_min = ds[lat_name].min().item()
    lat_max = ds[lat_name].max().item()
    
    # Check if the provided lon and lat are within the dataset ranges
    if lon < lon_min or lon > lon_max or lat < lat_min or lat > lat_max:
        data = (f"Provided lon and/or lat are outside the dataset range: "
                f"lon ({lon_min:.3f} to {lon_max:.3f}), lat ({lat_min:.3f} to {lat_max:.3f})")
    else:
        # Select the nearest point
        dsloc = ds.sel({lon_name: lon, lat_name: lat}, method='nearest')
        data = dsloc[variable].to_pandas().round(1)
        
    # Close the dataset
    ds.close()
    
    return data

# Example usage
# fname = "www/data/ncs/cmip6/pr/hist/pr_hist_month-10_19610101-20141231.nc"
# lon = 0
# lat = 46
# variable = 'pr'
# data, message = extract_point(fname, lon, lat, variable)
# if message:
#     print(message)

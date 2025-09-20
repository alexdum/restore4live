import xarray as xr
import numpy as np

def get_timestep_data(zarr_url, data_var=None, timestep_index=0):
    """
    Reads a specific timestep of data from a zarr store for a specified variable.

    Args:
        zarr_url (str): The URL to the zarr store.
        data_var (str, optional): The name of the data variable to extract.
                                  If None, the first data variable in the dataset is used.
        timestep_index (int, optional): The index of the timestep to extract. Defaults to 0 (first timestep).

    Returns:
        numpy.ndarray: A 2D NumPy array representing the selected timestep of the data.
                       Returns None if data variable is not found or an error occurs.
        numpy.ndarray: 1D NumPy array of x-coordinates.
        numpy.ndarray: 1D NumPy array of y-coordinates.
    """
    try:
        ds = xr.open_zarr(zarr_url)

        if data_var is None:
            # Get the name of the first data variable if not specified
            data_var_name = list(ds.data_vars.keys())[0]
        else:
            data_var_name = data_var

        if data_var_name not in ds.data_vars:
            print(f"Error: Data variable '{data_var_name}' not found in the zarr store.")
            return None, None, None

        # Select the specified timestep
        selected_timestep = ds[data_var_name].isel(time=timestep_index)

        # Extract data and coordinates
        data_values = selected_timestep.values
        x_coords = ds['x'].values
        y_coords = ds['y'].values

        return data_values, x_coords, y_coords
    except Exception as e:
        print(f"Error reading zarr data: {e}")
        return None, None, None

def get_zarr_timesteps(zarr_url):
    """
    Reads all timesteps from a zarr store.

    Args:
        zarr_url (str): The URL to the zarr store.

    Returns:
        list: A list of datetime objects representing the timesteps.
              Returns an empty list if no timesteps are found or an error occurs.
    """
    try:
        ds = xr.open_zarr(zarr_url)
        if 'time' in ds.coords:
            timesteps = ds['time'].dt.strftime('%Y-%m').values.tolist()
            return timesteps
        else:
            print("Warning: 'time' coordinate not found in the zarr store.")
            return []
    except Exception as e:
        print(f"Error reading zarr timesteps: {e}")
        return []

def get_point_timeseries(zarr_url, lon, lat, data_var=None):
    """
    Extracts a timeseries for a given point (lon, lat) from a zarr store.

    Args:
        zarr_url (str): The URL to the zarr store.
        lon (float): Longitude of the point.
        lat (float): Latitude of the point.
        data_var (str, optional): The name of the data variable to extract.
                                  If None, the first data variable in the dataset is used.

    Returns:
        dict: A dictionary with 'time' (list of dates) and 'values' (list of data values).
              Returns None if data variable is not found or an error occurs.
    """
    try:
        ds = xr.open_zarr(zarr_url)

        if data_var is None:
            data_var_name = list(ds.data_vars.keys())[0]
        else:
            data_var_name = data_var

        if data_var_name not in ds.data_vars:
            print(f"Error: Data variable '{data_var_name}' not found in the zarr store.")
            return None

        # Select the nearest point
        # Assuming 'x' and 'y' are longitude and latitude dimensions
        # If they are named differently, this needs adjustment
        point_data = ds[data_var_name].sel(x=lon, y=lat, method='nearest')

        # Extract time and values
        times = point_data['time'].dt.strftime('%Y-%m-%d').values.tolist()
        values = point_data.values.tolist()

        ds.close()
        return {'time': times, 'values': values}
    except Exception as e:
        print(f"Error extracting point timeseries from zarr: {e}")
        return None
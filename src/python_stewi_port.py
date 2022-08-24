import stewi
import pandas as pd
import numpy as np
import facilitymatcher
import re
from pathlib import Path

wd = '/home/mike/git/earthjustice/chem_risk_eval/data'

cities = pd.read_csv(Path(wd, 'general/cities.csv'),
                     dtype = {'EIS_FACILITY_ID': str})

chems = pd.read_csv(Path(wd, 'general/target_substances.csv'),
                    dtype = {'EIS_FACILITY_ID': str})

# fac_ids = pd.read_csv('/home/mike/git/earthjustice/stewi_comparison/nei_facilities_canceralley.csv',
#                       dtype = {'EIS_FACILITY_ID': str})

def clean_county_names(x):

    x = x.upper()
    x = re.sub('[\\. \\-]', '', x)
    x = re.sub('PARISH$', '', x)
    x = re.sub('COUNTY$', '', x)
    x = re.sub('THEBAPTIST$', '', x)
    x = re.sub('^SAINT', 'ST', x)

    return(x)

# years = [str(x) for x in range(2010, 2023)]
years = list(range(2010, 2023))
sources = ['NEI', 'TRI', 'DMR']
#----#

for source in sources:

    source_d = pd.DataFrame()

    for year in years:

        print(source + ', ' + str(year))
        try:
            d = stewi.getInventory(source, year)
        except:
            continue

        flows = stewi.getInventoryFlows(source, year)
        casrns = [str(x) for x in chems['CASRN_nohyphens'].tolist()]
        chems_included = flows[flows['FlowID'].isin(casrns)]['FlowName'].tolist()
        d = d.query('FlowName in @chems_included')
        flows.rename(columns={'FlowID':'CASRN'}, inplace=True)
        d = d.merge(flows[['CASRN', 'FlowName']], how='left', on='FlowName')
        chems['CASRN_nohyphens'] = chems['CASRN_nohyphens'].map(lambda x: str(x))
        d = d.merge(chems[['CASRN', 'CASRN_nohyphens', 'ej_name']], how='left', left_on='CASRN', right_on='CASRN_nohyphens')
        d = d.drop(['CASRN_x'], axis=1)
        d.rename(columns={'CASRN_y': 'CASRN'}, inplace=True)

        facilities = stewi.getInventoryFacilities(source, year)
        facilities = facilities.assign(County = lambda df: df['County'].map(lambda x: clean_county_names(x)))
        tx_counties = cities[cities['state'] == 'TX']['county'].tolist()
        la_parishes = cities[cities['state'] == 'LA']['county'].tolist()
        facilities = facilities.query('((State == "KY") & (County == "JEFFERSON")) |' +\
                                      '((State == "TX") & (County in @tx_counties)) |' +\
                                      '((State == "LA") & (County in @la_parishes))')

        d = facilities.merge(d, how='left', on='FacilityID')
        d = d.rename(columns={'State': 'state', 'County': 'county', 'CASRN': 'cas', 'Compartment': 'medium', 'FlowAmount': 'load_kg', 'Latitude': 'lat', 'Longitude': 'lon'})
        d['year'] = year
        d['source'] = source
        d['illegal'] = False
        d['location_set_to_county_centroid'] = False

        d = d[['year', 'state', 'county', 'cas', 'medium', 'load_kg', 'lat', 'lon', 'location_set_to_county_centroid', 'source', 'illegal']]
        d = d.dropna(subset=['load_kg'])
        source_d = source_d.append(d, ignore_index=True)

    src = source.lower()

    source_d.to_csv(Path(wd, src, src + '_joined2.csv'))
# nei['load_kg'].isnull().sum()
# nei['load_kg'].notnull().sum()

# fac_ids = pd.read_csv('/home/mike/git/earthjustice/stewi_comparison/tri_facilities_canceralley.csv',
#                       dtype = {'TRI_FACILITY_ID': str})

flows = stewi.getInventoryFlows('TRI', '2017')
nei['medium'].unique()
harmonize medium
flows[flows.FlowID == '79-00-5']
chem = tri[tri.FlowName == '1,1,2-Trichloroethane']

chem = chem[chem.FacilityID.isin(fac_ids.TRI_FACILITY_ID)]
# chem.FacilityID.astype(int).max()

chem.FlowAmount.sum()
chem.shape

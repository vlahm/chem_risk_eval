import stewi
import pandas as pd
import numpy as np
import facilitymatcher
import re

cities = pd.read_csv('/home/mike/git/earthjustice/chem_risk_eval/data/general/cities.csv',
                     dtype = {'EIS_FACILITY_ID': str})

chems = pd.read_csv('/home/mike/git/earthjustice/chem_risk_eval/data/general/target_substances.csv',
                    dtype = {'EIS_FACILITY_ID': str})

fac_ids = pd.read_csv('/home/mike/git/earthjustice/stewi_comparison/nei_facilities_canceralley.csv',
                      dtype = {'EIS_FACILITY_ID': str})

def clean_county_names(x):

    x = x.upper()
    x = re.sub('[\\. \\-]', '', x)
    x = re.sub('PARISH$', '', x)
    x = re.sub('COUNTY$', '', x)
    x = re.sub('THEBAPTIST$', '', x)
    x = re.sub('^SAINT', 'ST', x)

    return(x)

#----#

year = 2017
# tri = stewi.getInventory('TRI', 2017)
nei = stewi.getInventory('NEI', year)
# dmr = stewi.getInventory('DMR', 2017)

flows = stewi.getInventoryFlows('NEI', year)
casrns = [str(x) for x in chems['CASRN_nohyphens'].tolist()]
chems_included = flows[flows['FlowID'].isin(casrns)]['FlowName'].tolist()
nei = nei.query('FlowName in @chems_included')
nei.merge(chems, how='left', by='')
# flows[flows.FlowID == '75343']
# chem = nei[nei.FlowName == 'Ethylidene Dichloride']
#
# chem = chem[chem.FacilityID.isin(fac_ids.EIS_FACILITY_ID)]
# chem.FacilityID.astype(int).max()
#
# chem.FlowAmount.sum()
# chem.shape

# inventory_results = stewi.getInventory('NEI', year, 'flowbyfacility')
facilities = stewi.getInventoryFacilities('NEI', year)
facilities = facilities.assign(County = lambda df: df['County'].map(lambda x: clean_county_names(x)))
tx_counties = cities[cities['state'] == 'TX']['county'].tolist()
la_parishes = cities[cities['state'] == 'LA']['county'].tolist()
facilities = facilities.query('((State == "KY") & (County == "JEFFERSON")) |' +\
                              '((State == "TX") & (County in @tx_counties)) |' +\
                              '((State == "LA") & (County in @la_parishes))')

nei = facilities.merge(nei, how='left', on='FacilityID')

# fac_ids = pd.read_csv('/home/mike/git/earthjustice/stewi_comparison/tri_facilities_canceralley.csv',
#                       dtype = {'TRI_FACILITY_ID': str})

flows = stewi.getInventoryFlows('TRI', '2017')
flows[flows.FlowID == '79-00-5']
chem = tri[tri.FlowName == '1,1,2-Trichloroethane']

chem = chem[chem.FacilityID.isin(fac_ids.TRI_FACILITY_ID)]
# chem.FacilityID.astype(int).max()

chem.FlowAmount.sum()
chem.shape

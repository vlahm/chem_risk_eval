import stewi
import pandas as pd
import numpy as np
import facilitymatcher
import re
from pathlib import Path
from chemicalmatcher import get_program_synomyms_for_CAS_list
from stewicombo import combineFullInventories
from facilitymatcher import get_matches_for_id_list

wd = '/home/mike/git/earthjustice/chem_risk_eval/data'

cities = pd.read_csv(Path(wd, 'general/cities.csv'),
                     dtype = {'EIS_FACILITY_ID': str})
tx_counties = cities[cities['state'] == 'TX']['county'].tolist()
la_parishes = cities[cities['state'] == 'LA']['county'].tolist()

chems = pd.read_csv(Path(wd, 'general/target_substances.csv'),
                    dtype = {'EIS_FACILITY_ID': str})
chems['CASRN_nohyphens'] = chems['CASRN_nohyphens'].map(lambda x: str(x))

ej_cas = [str(x) for x in chems['CASRN'].tolist()]
chem_synonyms = get_program_synomyms_for_CAS_list(ej_cas, ['TRI', 'NEI', 'DMR'])

def clean_county_names(x):

    x = x.upper()
    x = re.sub('[\\. \\-]', '', x)
    x = re.sub('PARISH$', '', x)
    x = re.sub('COUNTY$', '', x)
    x = re.sub('THEBAPTIST$', '', x)
    x = re.sub('^SAINT', 'ST', x)

    return(x)

years = list(range(2010, 2023))
sources = ['NEI', 'TRI', 'DMR']
#----#
# source = 'TRI'; year = 2017
facils = pd.DataFrame()
for source in sources:

    src_facils = pd.DataFrame()

    for year in years:

        try:
            yr_facils = stewi.getInventoryFacilities(source, year)\
                .assign(County = lambda df: df['County'].map(lambda x: clean_county_names(x)))\
                .query('((State == "KY") & (County == "JEFFERSON")) |' +\
                       '((State == "TX") & (County in @tx_counties)) |' +\
                       '((State == "LA") & (County in @la_parishes))')

            src_facils = pd.concat([src_facils, yr_facils], axis=0, ignore_index=True)\
                .drop_duplicates()

        except:
            continue

    frs_ids = get_matches_for_id_list(source, src_facils['FacilityID'])
    src_facils = src_facils.merge(frs_ids[['FRS_ID', 'FacilityID']],
                                  on='FacilityID', how='left')\
        .rename(columns={'FRS_ID': 'FRS ID'})

    facils = pd.concat([facils, src_facils], axis=0, ignore_index=True)\
        .drop_duplicates(subset=['FacilityID'])

cmb = list()
cmb.append(combineFullInventories({'TRI':2010}, filter_for_LCI = False))
cmb.append(combineFullInventories({'TRI':2011, 'NEI':2011}, filter_for_LCI = False))
cmb.append(combineFullInventories({'TRI':2012, 'NEI':2012}, filter_for_LCI = False))
cmb.append(combineFullInventories({'TRI':2013, 'NEI':2013}, filter_for_LCI = False))
cmb.append(combineFullInventories({'TRI':2014, 'NEI':2014, 'DMR':2014}, filter_for_LCI = False))
cmb.append(combineFullInventories({'TRI':2015, 'NEI':2015, 'DMR':2015}, filter_for_LCI = False))
cmb.append(combineFullInventories({'TRI':2016, 'NEI':2016, 'DMR':2016}, filter_for_LCI = False))
cmb.append(combineFullInventories({'TRI':2017, 'NEI':2017, 'DMR':2017}, filter_for_LCI = False))
cmb.append(combineFullInventories({'TRI':2018, 'NEI':2018, 'DMR':2018}, filter_for_LCI = False))
cmb.append(combineFullInventories({'TRI':2019, 'DMR':2019}, filter_for_LCI = False))
cmb.append(combineFullInventories({'TRI':2020, 'DMR':2020}, filter_for_LCI = False))
cmb.append(combineFullInventories({'DMR':2021}, filter_for_LCI = False))
cmb.append(combineFullInventories({'DMR':2022}, filter_for_LCI = False))

# cmb = pd.concat([cmb1, cmb2, cmb3, cmb4, cmb5, cmb6, cmb7, cmb8, cmb9, cmb10, cmb11, cmb12, cmb13], axis=0)

cmb = pd.concat(cmb, axis=0)

cmb = cmb.merge(facils[['FacilityID', 'FRS ID', 'NAICS', 'City', 'County', 'State', 'Latitude', 'Longitude']],
                how='right', on='FacilityID')\
    .rename(columns={'State': 'state', 'County': 'county', 'SRS_CAS': 'cas',
                     'Compartment': 'medium', 'FlowAmount': 'load_kg',
                     'Latitude': 'lat', 'Longitude': 'lon',
                     'Source': 'source', 'Year': 'year', 'FRS ID': 'frs_id'})\
    .dropna(subset=['load_kg'])
cmb['illegal'] = False
cmb['location_set_to_county_centroid'] = False

cmb = cmb[['year', 'state', 'county', 'cas', 'frs_id', 'medium', 'load_kg', 'lat', 'lon', 'location_set_to_county_centroid', 'source', 'illegal']]

cmb = cmb.query('cas in @chems["CASRN"]')

cmb.query('source == "TRI"').to_csv(Path(wd, 'stewi_data_tri_joined2.csv'), index=False)
cmb.query('source == "DMR"').to_csv(Path(wd, 'stewi_data_dmr_joined2.csv'), index=False)
cmb.query('source == "NEI"').to_csv(Path(wd, 'stewi_data_nei_joined2.csv'), index=False)

#this way doesn't account for multireporting
# for source in sources:
#
#     source_d = pd.DataFrame()
#
#     for year in years:
#
#         print(source + ', ' + str(year))
#         try:
#             d = stewi.getInventory(source, year, US_States_Only = True)
#         except:
#             continue
#
#         flows = stewi.getInventoryFlows(source, year)
#
#         if source == 'NEI':
#             casrns = [str(x) for x in chems['CASRN_nohyphens'].tolist()]
#             chems_included = flows[flows['FlowID'].isin(casrns)]['FlowName'].tolist()
#             flows.rename(columns={'FlowID':'CASRN'}, inplace=True)
#         elif source == 'TRI':
#             casrns = [str(x) for x in chems['CASRN'].tolist()]
#             chems_included = flows[flows['CAS'].isin(casrns)]['FlowName'].tolist()
#             flows.rename(columns={'FlowID':'CASRN'}, inplace=True)
#         else: #DMR
#             flows = flows.drop(['CAS'], axis=1)\
#                 .merge(chem_synonyms[['CAS', source]], how='left',
#                        left_on='FlowName', right_on=source)\
#                 .dropna(how='all', subset=['CAS', 'DMR'])\
#                 .drop([source], axis=1)\
#                 .rename(columns={'CAS': 'CASRN'})
#             casrns = [str(x) for x in chems['CASRN'].tolist()]
#             chems_included = flows[flows['CASRN'].isin(casrns)]['FlowName'].tolist()
#
#         # src_chemnames = chem_synonyms[source][chem_synonyms[source].notnull()]
#         # chems_included = flows[flows['FlowName'].isin(src_chemnames)]['FlowName'].tolist()
#
#         d = d.query('FlowName in @chems_included')
#         # flows.rename(columns={'FlowID':'CASRN'}, inplace=True)
#         d = d.merge(flows[['CASRN', 'FlowName']], how='left', on='FlowName')
#
#         if source == 'NEI':
#             d = d.merge(chems[['CASRN', 'CASRN_nohyphens', 'ej_name']], how='left', left_on='CASRN', right_on='CASRN_nohyphens')
#             d = d.drop(['CASRN_x'], axis=1)
#             d.rename(columns={'CASRN_y': 'CASRN'}, inplace=True)
#         else:
#             d = d.merge(chems[['CASRN', 'CASRN_nohyphens', 'ej_name']], how='left', left_on='CASRN', right_on='CASRN')
#
#         facilities = stewi.getInventoryFacilities(source, year)
#         facilities = facilities.assign(County = lambda df: df['County'].map(lambda x: clean_county_names(x)))
#         facilities = facilities.query('((State == "KY") & (County == "JEFFERSON")) |' +\
#                                       '((State == "TX") & (County in @tx_counties)) |' +\
#                                       '((State == "LA") & (County in @la_parishes))')
#
#         if source == 'DMR':
#             frs_ids = get_matches_for_id_list('DMR', facilities['FacilityID'])
#             facilities = facilities.merge(frs_ids[['FRS_ID', 'FacilityID']],
#                                           on='FacilityID', how='left')\
#                 .rename(columns={'FRS_ID': 'FRS ID'})
#
#         d = facilities.merge(d, how='left', on='FacilityID')
#         d = d.rename(columns={'State': 'state', 'County': 'county', 'CASRN': 'cas', 'Compartment': 'medium', 'FlowAmount': 'load_kg', 'Latitude': 'lat', 'Longitude': 'lon'})
#         d['year'] = year
#         d['source'] = source
#         d['illegal'] = False
#         d['location_set_to_county_centroid'] = False
#
#         if source == 'DMR':
#             d = d[['year', 'state', 'county', 'cas', 'FRS ID', 'medium', 'load_kg', 'lat', 'lon', 'location_set_to_county_centroid', 'source', 'illegal']]
#         else:
#             d = d[['year', 'state', 'county', 'cas', 'medium', 'load_kg', 'lat', 'lon', 'location_set_to_county_centroid', 'source', 'illegal']]
#         d = d.dropna(subset=['load_kg'])
#         source_d = source_d.append(d, ignore_index=True)
#
#     src = source.lower()
#
#     source_d.to_csv(Path(wd, 'stewi_data', src + '_joined2.csv'))

#--- scraps
# source = 'NEI'
# chems_included_fulllist = list()
#
# for year in years:
#
#     print(source + ', ' + str(year))
#     try:
#         d = stewi.getInventory(source, year, US_States_Only = True)
#     except:
#         continue
#
#     flows = stewi.getInventoryFlows(source, year)
#     casrns = [str(x) for x in chems['CASRN_nohyphens'].tolist()]
#     chems_included = flows[flows['FlowID'].isin(casrns)]['FlowName'].tolist()
#     chems_included_fulllist.extend(chems_included)
#
# chems_included_fulllist = list(np.unique(np.array(chems_included_fulllist)))

cmb.query('source == "TRI" & state == "TX" & county != "JEFFERSON" & year == 2010').load_kg.sum()

all = combineFullInventories({'TRI':2014, 'NEI':2014, 'DMR':2014}, filter_for_LCI = False)
tri = combineFullInventories({'TRI':2014}, filter_for_LCI = False)
dmr = combineFullInventories({'DMR':2014}, filter_for_LCI = False)
nei = combineFullInventories({'NEI':2014}, filter_for_LCI = False)

all.query('Source == "TRI" & FlowName == "Formaldehyde"').FlowAmount.sum()
all.query('Source == "DMR" & FlowName == "Formaldehyde"').FlowAmount.sum()
all.query('Source == "NEI" & FlowName == "Formaldehyde"').FlowAmount.sum()
tri.query('FlowName == "Formaldehyde"').FlowAmount.sum()
dmr.query('FlowName == "Formaldehyde"').FlowAmount.sum()
nei.query('FlowName == "Formaldehyde"').FlowAmount.sum()

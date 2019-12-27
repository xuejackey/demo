import json
import argparse
import re
import copy
from pprint import pprint

obj_id = {
        'primaryOutcome': {'label':'pri', 'count':0},
        'secondaryOutcome': {'label':'sec', 'count':0},
        'otherOutcome': {'label':'oth', 'count':0},
        'exploratoryOutcome': {'label':'exp', 'count':0},
        'inclusionCriteria': {'label':'inc', 'count':0},
        'exclusionCriteria': {'label':'exc', 'count':0}
    }
    
def append_id(sections):
    id = copy.deepcopy(obj_id)
    for section in sections:
        objectiveType = section['objectiveType']
        o_id = id[objectiveType]

        section['id'] = o_id['label'] + "-" + str(o_id['count'])
        id[objectiveType]['count'] += 1
    
    return sections
    
def lambda_handler(event, context):
    print(f'event={event}')
    sections = event['Input']
    
    return append_id(sections)

import json
import argparse
import re
import copy
from pprint import pprint

rules = [{
            'objectiveType' : 'primaryOutcome' ,
            'patterns' : ['primary', 'objective']
        },
        {
            'objectiveType' : 'secondaryOutcome',
            'patterns' : ['second', 'objective']
        },
        {
            'objectiveType' : 'otherOutcome',
            'patterns' : ['objective']
        },
        {
            'objectiveType' : 'exploratoryOutcome',
            'patterns' : ['objective', 'explore']
        },
        {
            'objectiveType' : 'inclusionCriteria',
            'patterns' : ['inclusion', 'criteria']
        },
        {
            'objectiveType' : 'exclusionCriteria',
            'patterns' : ['exclusion', 'criteria']
        }
    ]


obj_id = {
        'primaryOutcome': {'label':'pri', 'count':0},
        'secondaryOutcome': {'label':'sec', 'count':0},
        'otherOutcome': {'label':'oth', 'count':0},
        'exploratoryOutcome': {'label':'exp', 'count':0},
        'inclusionCriteria': {'label':'inc', 'count':0},
        'exclusionCriteria': {'label':'exc', 'count':0}
    }


def all_elements_exist(string, lst):
    print(f'string={string} and list={lst}')
    for element in lst:
        if not re.search(element, string, re.IGNORECASE):
            print('return false')
            return False
    return True


def find_group(section):
    for rule in rules:
        if all_elements_exist(section['title'], rule['patterns']):
            return rule['objectiveType']
    return None
    
def identify_type(sections):
    """
    Description: identify the section type using the 'title' field and return all sections with valid types.
    Parameters
    ----------
        sections: list dict objects with 
        'Filename', 'level', 'level_number', 'title', 'content'

    Return
    ----------
        keep_sections: list of dict objects with 
        'Filename', 'level', 'level_number', 'title', 'content' 'objectiveType'
    """
    if sections is None: return list()
    if type(sections) is not list:  sections = [sections]


    keep_sections = list()
    for section in sections:
        otype = find_group(section)
        if otype is not None: 
            section['objectiveType'] = otype
            keep_sections.append(section)
    return keep_sections
    
def lambda_handler(event, context):
    # TODO implement
    print(f'event={event}')
    sections = event['Input']
    
    return identify_type(sections) 
    #return {
    #    'statusCode': 200,
    #    'keep_sections': json.dumps('Hello from Lambda!')
    #}

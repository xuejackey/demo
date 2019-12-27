import json

def add_fields(sections, objectiveType, fields):
    for section in sections:
        if section['objectiveType'] in objectiveType:
            section.update(fields)
    return sections
    
def lambda_handler(event, context):
    print(f'event={event}')
    sections = event['Input']
    
    objectiveTypes =  ['primaryOutcome', 'secondaryOutcome', 'otherOutcome', 'exploratoryOutcome']
    return add_fields(sections, objectiveTypes, {'detection':'free_text', 'endpoint':''})

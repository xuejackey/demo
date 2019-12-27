import json

def pop(sections, keys):
    for section in sections:
        for key in keys:
            section.pop(key,None)
            section.pop(key,None)
    return(sections)

def lambda_handler(event, context):
    print(f'event={event}')
    sections = event['Input']
    
    return pop(sections,['level_number', 'level', 'Filename'])
    
    

# Trigger: arn:aws:sns:us-east-2:608494368293:textract-pipleline-status-topic

import json
import boto3
import time


def getJobResults(jobId):
    pages = []
    client = boto3.client('textract')
    print("get jobId:" + jobId)
    response = client.get_document_text_detection(JobId=jobId)
    print("response: {}".format(response))
    pages.append(response)
    print("Resultset page recieved: {}".format(len(pages)))
    nextToken = None
    if('NextToken' in response):
        nextToken = response['NextToken']

    while(nextToken):
        # time.sleep(5)

        response = client.get_document_text_detection(JobId=jobId, NextToken=nextToken)

        pages.append(response)
        print("Resultset page recieved: {}".format(len(pages)))
        nextToken = None
        if('NextToken' in response):
            nextToken = response['NextToken']

    return pages
    
def save2s3(res, documentName):
    s3BucketName="textract-pileline-doc"
    documentName += ".json"
    
    s3_client = boto3.client('s3')
    response = s3_client.put_object(Bucket=s3BucketName, Key=documentName, Body=json.dumps(res))


def lambda_handler(event, context):
    # TODO implement
    # print("request: {}".format(event))
    
    # msg = event['Records'][0]['Sns']['Message']
    # print(msg)
    # data = json.loads(msg)
    # print('data--------')
    # print(data)
    # data = event['Input']
    job_id = event['Input']
    print(job_id)
    # job_id = '7b1123bdc91eef787c370c13588e4ca84af2721ccd6274182882ed166b01fd80'
    
    res = getJobResults(job_id)
    
    save2s3(res, 'result/'+ job_id[:20])
    
    return {
        'statusCode': 200,
        'body': json.dumps('Try to get textract job result!')
    }

{
    "keyWA": "{{configsrcW.keyA}}"
    ,"keyWB": "{{configsrcW.keyB}}"
    ,"keyXA": "{{configsrcX.keyA}}"
    ,"keyXB": "{{configsrcX.keyB}}"
    ,"keyYA": "{{configsrcY.keyA}}"
    ,"keyYB": "{{configsrcY.keyB}}"

{{#queue.active}}
    ,"queueUrl": "{{queue.url}}"
{{/queue.active}}
{{^queue.active}}
    ,"queueUrl": null
{{/queue.active}}

{{#processor.size}}
    ,"jobsize": "{{processor.size}}"
{{/processor.size}}

}

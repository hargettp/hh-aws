(in-package :hh-aws)

(export
 (list

  'ec2-list-regions
  'ec2-list-availability-zones
  'ec2-list-instances
  'ec2-describe-security-groups
  )
 )

(defservice ec2
  :endpoint ( 
	     ;; TODO hmm, ec2 has multiple, region-specific endpoints
	     ;; ec2.us-east-1.amazonaws.com
	     ;; ec2.us-west-1.amazonaws.com
	     ;; ec2.eu-west-1.amazonaws.com
	     ;; ec2.ap-southeast-1.amazonaws.com
             (string "ec2.amazonaws.com")
             )
  :version ( 
            (string "2010-06-15") 
            )
  :request (
	    ec2-request
	    :slots (
		    (region
		     :initform nil
		     :initarg :region
		     :accessor region-for
		     )
		    )
	    :init (progn
		    (add-parameter some-request "Action" (default-action some-request))
		    (add-parameter some-request "AWSAccessKeyId" (access-key-id *credentials*))
		    (add-parameter some-request "SignatureMethod" "HmacSHA256")
		    (add-parameter some-request "SignatureVersion" "2")
		    (add-parameter some-request "Version" (version-of (service-of some-request)))
		    (add-parameter some-request "Timestamp" (aws-timestamp))
		    )
	    :endpoint (progn
			(if (region-for some-request)
			    (region-for some-request)
			    (call-next-method)
			    )  
			)
	    :signed-parameters (progn
				 (cons (cons "Signature" (request-signature some-request))
				       (sorted-parameters some-request)
				       )
				 )
	    )
  )

(defrequest ec2-list-regions
    :documentation "List all regions"
    :bases (ec2-request)
    :service ec2
    :action ( 
	     (string "DescribeRegions")
	     )
    :result-format (
		    `(
		      "regionName"
		      "regionEndpoint"
		      )
		     )
    )

(defxmlparser ec2-instances-parser builder
  :enter (
          (call-next-method)
          )
  :text (
         (if (path-p '("reservationSet" "item" "instancesSet" "item" "instanceId"))
             (progn
               (putend text-string (current-of handler) )
               )
             )
         (call-next-method)
         )
  ;; :exit (
  ;;        (if (path-p '("Contents"))
  ;;            (progn
  ;;              (putend (current-of handler) (results-of handler) )
  ;;              (setf (current-of handler) nil)
  ;;              )
  ;;            )
  ;;        (call-next-method)
  ;;        )
  :finish (
           (results-of handler)
           )
  )

(defrequest ec2-list-availability-zones
  :documentation "List all availability zones"
  :bases (ec2-request)
  :service ec2
  :action ( 
           (string "DescribeAvailabilityZones")
           )
  :result-format (
                  `(
                    "zoneName"
		    "zoneState"
		    "regionName"
		    "messageSet"
                    )
                  )

  )

(defrequest ec2-list-instances
  :documentation "List all regions"
  :bases (ec2-request)
  :service ec2
  :action ( 
           (string "DescribeInstances")
           )
  :args (
	 region-name
	 )
  :call (
         (setf (region-for some-request) region-name)
         (call-next-method)
         )
  :result-format (
                  `(
                    "instanceId"
		    "imageId"
		    ;; "instanceState"
		    "privateDnsName"
		    "dnsName"
		    ;; "reason"
		    "keyName"
		    "amiLaunchIndex"
		    ;; "productCodes"
		    "instanceType"
		    "launchTime"
		    ;; "placement"
		    "kernelId"
		    ;; "monitoring"
		    "privateIpAddress"
		    "ipAddress"
		    "architecture"
		    "rootDeviceType"
		    ;; "blockDeviceMapping"
		    "virtualizationType"
                    )
                  )
  :result (
	   (call-next-method)
	   )
    )

(defrequest ec2-describe-security-groups
    :documentation "List all security groups"
    :bases (ec2-request)
    :service ec2
    :action ( 
	     (string "DescribeSecurityGroups")
	     )
  :args (
	 region-name
	 )
  :call (
         (setf (region-for some-request) region-name)
         (call-next-method)
         )
    )
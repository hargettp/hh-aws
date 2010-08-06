(in-package :hh-aws)

(export
 (list

  'ec2-list-regions
  'ec2-list-availability-zones
  'ec2-list-instances

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
  )

 (defrequest ec2-list-regions
  :documentation "List all regions"
  :bases (ec2-request)
  :service ec2
  :action ( 
           (string "DescribeRegions")
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

;; <amiLaunchIndex>0</amiLaunchIndex>
;; <productCodes/>
;; <instanceType>m1.small</instanceType>
;; <launchTime>2010-07-13T13:25:38.000Z</launchTime>
;; <placement>
;;     <availabilityZone>us-east-1a</availabilityZone>
;;     <groupName/>
;; </placement>
;; <kernelId>aki-754aa41c</kernelId>
;; <monitoring>
;;     <state>disabled</state>
;; </monitoring>
;; <privateIpAddress>10.241.22.16</privateIpAddress>
;; <ipAddress>75.101.150.79</ipAddress>
;; <architecture>i386</architecture>
;; <rootDeviceType>instance-store</rootDeviceType>
;; <blockDeviceMapping/>
;; <virtualizationType>paravirtual</virtualizationType>


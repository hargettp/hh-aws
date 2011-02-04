;; Copyright (c) 2010-11 Phil Hargett

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(in-package :hh-aws)

(export
 (list

  'ec2-describe-regions
  'ec2-describe-availability-zones
  'ec2-describe-instances
  'ec2-describe-security-groups
  'ec2-authorize-security-group-ingress
  'ec2-revoke-security-group-ingress

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

(defrequest ec2-describe-regions
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
  :finish (
           (results-of handler)
           )
  )

(defrequest ec2-describe-availability-zones
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

(defrequest ec2-describe-instances
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

(defclass security-group-builder (builder)
  (
   (current-ip-permission :initform nil :accessor current-permission-of)
   )
  )

(defxmlparser security-groups-parser security-group-builder
  :enter (
          (push (symbol-name name) *current-elements*)
	  (when (path-p '("item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
	    (putend :group (current-of handler))
	      )
	  (when (path-p '("item" "ipPermissions" "item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
	    (putend `(:permission) (current-permission-of handler))
	      )
          )
  :text (
	 ;; top-level security group info
	 (if (path-p '("ownerId" "item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
             (progn
               (putend `(:owner ,text-string) (current-of handler) )
               )
             )
         (if (path-p '("groupName" "item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
             (progn
               (putend `(:name ,text-string) (current-of handler) )
               )
             )
	 (if (path-p '("groupDescription" "item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
             (progn
               (putend `(:description ,text-string) (current-of handler) )
               )
             )
	 (when (path-p '("ipProtocol" "item" "ipPermissions" "item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
	   (putend `(:ip-protocol , text-string) (current-permission-of handler) )
	   )
	 (when (path-p '("fromPort" "item" "ipPermissions" "item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
	   (putend `(:from-port , text-string) (current-permission-of handler) )
	   )
	 (when (path-p '("toPort" "item" "ipPermissions" "item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
	   (putend `(:to-port , text-string) (current-permission-of handler) )
	   )
	 (when (path-p '("groups" "item" "ipPermissions" "item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
	   (putend `(:groups , text-string) (current-permission-of handler) )
	   )
	 ;; ip range info
	 (when (path-p '("cidrIp" "item" "ipRanges" "item" "ipPermissions" "item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
	   (putend `(:ip-range , text-string) (current-permission-of handler) )
	   )
         (call-next-method)
         )
  :exit (
	 (when (path-p '("item" "ipRanges" "item" "ipPermissions" "item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
	   (putend `(:ip-range ,(current-permission-of handler)) (current-of handler) )
	   (setf (current-permission-of handler) nil)
	   )
	 (when (path-p '("item" "securityGroupInfo" "DescribeSecurityGroupsResponse"))
	   (putend (current-of handler) (results-of handler))
	   (setf (current-of handler) nil)
	   )
	 (pop *current-elements*)
	 )
  :finish (
           (results-of handler)
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
  :result (
	   ;; (values 
	    (with-input-from-string (is (response-body some-response))
	      (security-groups-parser is)
	      )
	    ;; (call-next-method)	   
	    ;; )
	   )
    )

(defrequest ec2-authorize-security-group-ingress
    :documentation""
    :bases (ec2-request)
    :service ec2
    :action (
	     (string "AuthorizeSecurityGroupIngress")
	     )
    :parameters (
		 ("GroupName" . groupName)
		 ("IpPermissions.1.IpProtocol" . protocol)
		 ("IpPermissions.1.FromPort" . from-port)
		 ("IpPermissions.1.ToPort" . to-port)
		 ("IpPermissions.1.IpRanges.1.CidrIp" . cidr-ip)
		 )
    )

(defrequest ec2-revoke-security-group-ingress
    :documentation""
    :bases (ec2-request)
    :service ec2
    :action (
	     (string "RevokeSecurityGroupIngress")
	     )
    :parameters (
		 ("GroupName" . groupName)
		 ("IpProtocol" . protocol)
		 ("FromPort" . from-port)
		 ("ToPort" . to-port)
		 ("CidrIp" . cidr-ip)
		 )
    )
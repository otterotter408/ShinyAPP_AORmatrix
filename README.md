# ShinyAPP_AORmatrix

# This is a R shiny Application that used as an external data management tool that is outside of HR system.
# It allows users to add or modify the area of responsilibity (AOR) data in the datatable, and save changes.
# AOR Data is stored in data warehouse tables. 

# Adding
# When user manages the AOR,  country, leader, HRBP , and start date can be selected from the dropdown list.
# dropdown selections come from data warehouse table that is refreshed daily to populate active employees only.

# Retiring
# Existing AOR can be retired using "End AOR" button.  user can also choose the "End Date" in this step.

# Security 
# Security is controled using shiny sessions in two ways.
##  1.users are grouped into editors and visitor. Only editors have permission to edit/save datatable.
##  2.editor activities are tracked. Each button an editor selected will leave a fingerprint (login email) of the edit in column "Updateby"


# Backend validation/processing
##  APP will check what's been changed against system AOR records, generate a dataset for what needs to be added/removed in system AOR.
##  The dataset will be uploaded to HR AOR system through API.(WIP)

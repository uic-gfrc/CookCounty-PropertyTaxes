*******************************************************************
* this program written by David Merriman on 303_31_2025 to read ptab data
**************************************************************************;
*****************************************************
* boiler plate code
*****************************************
set more off
clear
set memory 200m
di c(current_date)
di c(current_time) 
di c(sysdir_personal) 
di c(pwd)
di c(adopath) 
*scalar location="laptop"
*scalar location="loyola"
*scalar location="igpa"
scalar list
#delimit cr
******************************************************
* read in data
**************
*import excel "C:\Users\dmerrim\OneDrive - University of Illinois Chicago\igpa\cook county board\property tax taskforce\2025\PTAB\Levy Adjustment 2024 for PA 102-0519.xlsx", sheet("2024 Levy Adjust PA102-0519")

cd "C:\Users\dmerrim\OneDrive - University of Illinois Chicago\igpa\cook county board\property tax taskforce\2025\PTAB\"
dir
import excel "Levy Adjustment 2024 for PA 102-0519.xlsx", sheet("2024 Levy Adjust PA102-0519") first
***********************************************************************************
* source of data
*this workbook downloaded by David Merriman on March 24, 2025 
*from  https://www.cookcountyclerkil.gov/property-taxes/tax-extension-and-rates
*
*Levy Adjustment PA 102-0519 (Excel)
*
*only 1 year of data was available.
***************************************************************************
gen refund_share=AggregateRefunds/TotalTax
gen refund_share_pc=100* refund_share
format refund_share_pc %3.1f
centile refund_share_pc, centile(5(10)95)
sort refund_share
gen cum_AggregateRefunds =sum(AggregateRefunds)/1000000
format cum_AggregateRefunds %6.2fc
format AggregateRefunds %12.0fc


list AgencyName refund_share_pc AggregateRefunds cum_AggregateRefund if refund_share_pc~=.
count if refund_share_pc==.
centile refund_share_pc, centile(5(10)95)

exit



egen totaltax=total(TotalTax)

egen totrefunds=total(AggregateRefunds)

gen share_refunds= totrefunds/ totaltax

exit









clear;
set obs 56 ;   
gen x = _n   ;
*expand 10;
tab x;
list;
gen quantity=x-1;
gen p_demand_1=max(0,(50/.75)-(1/.75)*quantity) if quantity<=50;
gen p_demand_2=max(0,(20/0.5)-(1/0.5)*quantity) if quantity<=20;
twoway line p_demand_1 p_demand_2 quantity, lpattern(shortdash longdash) 
xline(7.5 20 31.33,lpattern(dash)) yline(25 40) 
ytitle("price") xtitle("quantity")title("Two demand curves for a pure public good")
subtitle("horizontal lines are possible marginal cost")note("Source:hypothetical numbers")
legend(label(1 "Person_1_Demand") label(2 "Person_2_Demand")) xlabel(0 7.5 20 31.33 40 50) ylabel(0 10 25 40 (10) 70);
;
exit;


/*
gen price=x;
gen q_demand_1=max(0,(50-.75*price));
gen q_demand_2=max(0,(20-.5*price));
gen marginal_cost=25;
*gen vertical_line_1=7.5;
*twoway (connected price q_demand_1);
twoway (connected price q_demand_1 if price<=66.66666)|| (connected price q_demand_2 if price<=40)|| (connected marginal_cost q_demand_1),
xline(7.5 31.75,lpattern(dash))
ytitle("price") xtitle("quantity")title("Two demand curves for a public good")
subtitle("Constant marginal cost at $25")note("Source:hypothetical numbers")
legend(label(1 "Person_1_Demand") label(2 "Person_2_Demand")) xlabel(0(2)50) ylabel(0(10)80);
*/




*lstyle(p1 p2 p3)

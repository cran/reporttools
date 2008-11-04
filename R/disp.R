`disp` <-
function (n, d = 2) 
{
    t <- format(round(as.numeric(n), d), nsmall = d)
    return(t)
}

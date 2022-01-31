/*The following code is just a back up. I submitted my code and resulting answers via the Google Forms.*/

/* Question 1 */

SELECT COUNT(DISTINCT CustomerName) FROM Customers
WHERE Country = 'USA'

/* Answer 13 */


/* Question 2 */
SELECT COUNT(DISTINCT City) FROM Customers
WHERE Country = 'USA'

UNION SELECT COUNT(DISTINCT City) FROM Customers
WHERE NOT Country = 'USA'

/* Answer 12, 57 */

/*Question 3*/

SELECT Customers.City
FROM ((Orders
INNER JOIN Customers ON Orders.CustomerID = Customers.CustomerID)
INNER JOIN OrderDetails ON Orders.OrderID = OrderDetails.OrderID)
GROUP BY City
ORDER BY Quantity DESC
LIMIT 5;

/* Answer
Seattle
München
I. de Margarita
Graz
Sevilla */

/*Question 4*/

SELECT Customers.City, (SUM(OrderDetails.Quantity) / COUNT(Orders.OrderID)) AS UPT
FROM ((Orders
INNER JOIN Customers ON Orders.CustomerID = Customers.CustomerID)
INNER JOIN OrderDetails ON Orders.OrderID = OrderDetails.OrderID)
WHERE Country = 'USA'
GROUP BY City
ORDER BY UPT DESC
LIMIT 5;

/*Answer
Boise
Seattle
Anchorage
Albuquerque
Lander*/
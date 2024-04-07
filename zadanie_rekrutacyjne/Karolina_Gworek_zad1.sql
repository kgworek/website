-- Create a query which will return data about the total weight per product ordered by
-- the customer with C delivered on February 13, 2024. The query should
-- return data in the following schema:
-- sorted by totalWeight ascending.

SELECT p.product_id AS productID, sum(p.weight*op.quantity) AS totalWeight
FROM  [dbo].[products] p
JOIN [dbo].[orders_products] op ON p.[product_id] = op.[product_id] 
JOIN [dbo].[orders] o ON o.[order_id] = op.[order_id]
JOIN [dbo].[route_segments] r ON o.order_id = r.order_id
WHERE o.[customer_id] = 32 
AND CONVERT(DATE, r.segment_end_time) = '2024-02-13'
GROUP BY p.product_id, p.weight
ORDER BY totalWeight ASC;


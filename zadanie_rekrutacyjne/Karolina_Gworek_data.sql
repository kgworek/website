select * 
from [dbo].[route_segments] rs  
left join [dbo].[orders] o on rs.[order_id] = o.[order_id] 
where segment_type='STOP'


select count(*)
from [dbo].[route_segments] rs  
left join [dbo].[orders] o on rs.[order_id] = o.[order_id] 
where segment_type='STOP' and rs.order_id IS NULL

select count(*)
from [dbo].[route_segments] rs  
left join [dbo].[orders] o on rs.[order_id] = o.[order_id] 
where segment_type='STOP'

select *
from [dbo].[route_segments] rs  
join [dbo].[orders] o on rs.[order_id] = o.[order_id] 

select [segment_id], [driver_id], [segment_type], [segment_start_time], [segment_end_time], rs.[order_id], p.[product_id], [quantity]*[weight] as 'total_weight'
from [dbo].[route_segments] rs  
join [dbo].[orders_products] op on rs.[order_id] = op.[order_id] 
join [dbo].[products] p on p.[product_id]=op.[product_id]

select [segment_id], [driver_id], rs.[order_id], p.[product_id], [quantity]*[weight] as 'total_weight', (DATEDIFF(SECOND, rs.segment_start_time, rs.segment_end_time)) as 'actual_delvery_sec'
from [dbo].[route_segments] rs  
join [dbo].[orders_products] op on rs.[order_id] = op.[order_id] 
join [dbo].[products] p on p.[product_id]=op.[product_id]
WHERE DATEDIFF(SECOND, rs.segment_start_time, rs.segment_end_time) > 1500


SELECT o.sector_id, AVG(o.planned_delivery_duration) AS mean_planned_delivery_duration,  AVG(DATEDIFF(SECOND, rs.segment_start_time, rs.segment_end_time)) AS mean_delivery_duration_seconds
FROM dbo.route_segments rs
JOIN dbo.orders o ON rs.order_id = o.order_id
WHERE DATEDIFF(SECOND, rs.segment_start_time, rs.segment_end_time) < 1500
GROUP BY o.sector_id;


SELECT rs.[driver_id], AVG(o.planned_delivery_duration) AS mean_planned_delivery_duration,  AVG(DATEDIFF(SECOND, rs.segment_start_time, rs.segment_end_time)) AS mean_delivery_duration_seconds
FROM dbo.route_segments rs
JOIN dbo.orders o ON rs.order_id = o.order_id
--WHERE DATEDIFF(SECOND, rs.segment_start_time, rs.segment_end_time) < 1500
GROUP BY rs.[driver_id];

SELECT rs.[driver_id], rs.[order_id],   p.[product_id],  [quantity] * [weight] AS 'total_weight',   DATEDIFF(SECOND, rs.segment_start_time, rs.segment_end_time) AS 'actual_delivery_sec'
FROM  [dbo].[route_segments] rs  
JOIN  [dbo].[orders_products] op ON rs.[order_id] = op.[order_id] 
JOIN  [dbo].[products] p ON p.[product_id] = op.[product_id];


SELECT rs.[order_id], sum(op.[quantity]) as 'quantity of products', SUM(op.[quantity] * p.[weight]) AS total_weight, AVG(DATEDIFF(SECOND, rs.segment_start_time, rs.segment_end_time)) AS actual_delivery_sec
FROM  [dbo].[route_segments] rs  
JOIN [dbo].[orders_products] op ON rs.[order_id] = op.[order_id] 
JOIN  [dbo].[products] p ON p.[product_id] = op.[product_id]
GROUP BY rs.[order_id]
order by rs.[order_id];

select [order_id],  (DATEDIFF(SECOND, segment_start_time, segment_end_time)) as 'actual_delvery_sec'
from [dbo].[route_segments]
where  (DATEDIFF(SECOND, segment_start_time, segment_end_time)) >1500 
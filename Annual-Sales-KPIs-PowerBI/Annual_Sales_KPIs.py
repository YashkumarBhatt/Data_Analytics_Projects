import csv
import random
from datetime import datetime
from faker import Faker

fake = Faker()

# Create realistic shampoo data
products = [
    {"id": "SHMP001", "name": "Herbal Bliss", "category": "Herbal", "unit_price": 12.99, "cost": 5.50},
    {"id": "SHMP002", "name": "Dandruff Defense", "category": "Anti-Dandruff", "unit_price": 15.50, "cost": 6.80},
    {"id": "SHMP003", "name": "Silky Smooth", "category": "Moisturizing", "unit_price": 10.99, "cost": 4.75},
    {"id": "SHMP004", "name": "Keratin Repair", "category": "Repair", "unit_price": 18.75, "cost": 8.20},
    {"id": "SHMP005", "name": "Coconut Nourish", "category": "Natural", "unit_price": 13.25, "cost": 5.90},
    {"id": "SHMP006", "name": "Tea Tree Fresh", "category": "Clarifying", "unit_price": 11.45, "cost": 4.95},
    {"id": "SHMP007", "name": "Argan Oil Therapy", "category": "Luxury", "unit_price": 16.99, "cost": 7.30},
    {"id": "SHMP008", "name": "Volume Boost", "category": "Volumizing", "unit_price": 14.50, "cost": 6.25},
    {"id": "SHMP009", "name": "Baby Gentle", "category": "Kids", "unit_price": 8.99, "cost": 3.80},
    {"id": "SHMP010", "name": "Scalp Soother", "category": "Medicated", "unit_price": 17.25, "cost": 7.50}
]

# Generate sales data for 12 months
sales_data = []
start_date = datetime(2023, 1, 1)

for month in range(12):
    current_date = (start_date.month + month - 1) % 12 + 1
    year = 2023 + (start_date.month + month - 1) // 12
    date_sold = datetime(year, current_date, 1).strftime("%Y-%m-%d")
    
    for product in products:
        quantity = random.randint(50, 500)
        cogs = round(product["cost"] * quantity, 2)
        profit = round((product["unit_price"] - product["cost"]) * quantity, 2)
        
        sales_data.append({
            "Item ID": product["id"],
            "Shampoo Name": product["name"],
            "Category": product["category"],
            "Date Sold": date_sold,
            "Quantity Sold": quantity,
            "Unit Price": product["unit_price"],
            "COGS": cogs,
            "Profit": profit
        })

# Shuffle the data to make it more realistic
random.shuffle(sales_data)

# Save to CSV
filename = "bapa_sitaram_sales.csv"
fields = ["Item ID", "Shampoo Name", "Category", "Date Sold", 
          "Quantity Sold", "Unit Price", "COGS", "Profit"]

with open(filename, "w", newline="") as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=fields)
    writer.writeheader()
    writer.writerows(sales_data)

print(f"Sales data successfully saved to {filename}")
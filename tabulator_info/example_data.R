# Create a comprehensive test data frame with various data types and edge cases
dat <- data.frame(
    # Numeric types
    id = 1:8,
    age = c(25, 30, NA, 35, 28, 42, NA, 31),
    price = c(19.99, 29.50, 15.75, NA, 11145.00, 12.99, 89.99, NA),
    percent = c(85.5, NA, 92.0, 78.5, 88.0, NA, 95.5, 82.0),

    # Character types with problematic quotes
    name = c(
        "Alice Johnson",
        "Bob \"The Builder\" Smith",
        "Carol O'Connor",
        "David \"Dave\" Wilson",
        "Eve \"Evie\" Brown",
        "Frank \"Frankie\" Davis",
        "Grace \"Gracie\" Miller",
        "Henry \"Hank\" Taylor"
    ),
    email = c(
        "alice@example.com",
        "bob.smith@company.org",
        NA,
        "david.wilson@test.net",
        "eve.brown@demo.com",
        NA,
        "grace.miller@sample.edu",
        "henry.taylor@mail.co.uk"
    ),
    url = c(
        "https://example.com/alice",
        "http://company.org/bob",
        "https://test.net/carol",
        NA,
        "https://demo.com/eve",
        "http://sample.edu/frank",
        NA,
        "https://mail.co.uk/henry"
    ),

    # Logical types
    active = c(TRUE, FALSE, NA, TRUE, FALSE, TRUE, NA, FALSE),
    verified = c(TRUE, TRUE, FALSE, NA, TRUE, FALSE, TRUE, TRUE),

    # Date types
    created_date = as.Date(c(
        "2023-01-15",
        "2023-02-20",
        "2023-03-10",
        "2023-04-05",
        "2023-05-12",
        "2023-06-18",
        "2023-07-22",
        "2023-08-30"
    )),
    last_login = as.POSIXct(c(
        "2023-12-01 09:30:00",
        "2023-12-02 14:15:00",
        "2023-12-03 11:45:00",
        "2023-12-04 16:20:00",
        "2023-12-05 08:30:00",
        "2023-12-06 13:45:00",
        "2023-12-07 10:15:00",
        "2023-12-08 15:30:00"
    )),

    # Factor types
    category = factor(c("A", "B", NA, "A", "C", NA, "B", "A")),
    status = factor(c(
        "active",
        "inactive",
        "pending",
        "active",
        "suspended",
        "active",
        "inactive",
        "pending"
    )),

    # Integer types
    login_count = c(15L, 8L, NA, 23L, 12L, 5L, 31L, 18L),
    score = c(95L, 87L, 92L, NA, 78L, 89L, 96L, 83L),

    # Mixed problematic content
    notes = c(
        "Regular user, no issues",
        "User with \"special\" requirements",
        NA,
        "Customer said \"everything is great\"",
        "Account \"suspended\" temporarily",
        "User \"Bob\" mentioned \"problems\"",
        NA,
        "All \"quotes\" should be \"handled\" properly"
    ),

    stringsAsFactors = FALSE
)

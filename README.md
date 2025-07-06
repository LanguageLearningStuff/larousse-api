# Larousse Dictionary API 🇫🇷📚

Welcome to the Larousse Dictionary API! This project is a Spring Boot application that lets you search for French word
definitions and examples, scraped directly from the Larousse website. Perfect for language learners, developers, and
anyone who loves words.

## 🚀 Features

- Search for French word definitions and examples
- RESTful API built with Spring Boot
- Live data scraping from Larousse using Jsoup

## 🛠️ Installation

1. **Clone this repository:**
   ```bash
   git clone http://github.com/LanguageLearningStuff/larousse-api
   cd larousse-api
   ```
2. **Run the application locally:**
   ```bash
   ./gradlew bootRun
   ```

The app will start on `http://localhost:8080` by default.

## 🔎 Example Usage

To search for a definition, call the `/api/v1/search` endpoint with a `q` parameter:

```bash
curl "http://localhost:8080/api/v1/search?q=bonjour"
```

**Response:**

```json
[
  {
    "definition": "Salutation adressée à quelqu'un, le matin ou dans la journée.",
    "example": "Elle lui dit bonjour en souriant."
  }
]
```

## 🤝 Contributing

Pull requests are welcome! For major changes, please open an issue first to discuss what you would like to change.

## 📄 License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

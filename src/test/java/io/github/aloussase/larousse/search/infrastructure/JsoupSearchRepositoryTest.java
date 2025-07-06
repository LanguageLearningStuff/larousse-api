package io.github.aloussase.larousse.search.infrastructure;

import io.github.aloussase.larousse.search.config.SearchConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.io.ResourceLoader;
import org.wiremock.spring.EnableWireMock;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static org.assertj.core.api.Assertions.assertThat;

@EnableWireMock
@SpringBootTest(classes = {SearchConfig.class, JsoupSearchRepository.class, ResourceLoader.class})
class JsoupSearchRepositoryTest {

    @Value("${wiremock.server.baseUrl}")
    private String wireMockServer;

    @Autowired
    private ResourceLoader rl;

    private String htmlDoc;

    @BeforeEach
    void setUp() throws IOException {
        if (htmlDoc != null) return;
        final var r = rl.getResource("classpath:chat.html");
        htmlDoc = r.getContentAsString(StandardCharsets.UTF_8);
    }

    @Test
    void testFetchingDefinitionsReturnsExpectedListForGivenHtmlDocument() {
        // Arrange
        final var searchTerm = "chat";

        stubFor(get("/" + searchTerm).willReturn(
                aResponse()
                        .withBody(htmlDoc)
                        .withHeader("Content-Type", "text/html")
                        .withStatus(200)
        ));

        final var config = new SearchConfig();
        config.setLarousseBaseUrl(wireMockServer);

        final var repository = new JsoupSearchRepository(config);

        // Act
        final var defs = repository.search(searchTerm);

        // Assert
        assertThat(defs).hasSize(8);
    }

}
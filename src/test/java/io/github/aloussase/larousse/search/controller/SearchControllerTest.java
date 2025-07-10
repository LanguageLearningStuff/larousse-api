package io.github.aloussase.larousse.search.controller;

import io.github.aloussase.larousse.core.exception.handler.GlobalExceptionHandler;
import io.github.aloussase.larousse.search.domain.entity.Definition;
import io.github.aloussase.larousse.search.domain.repository.SearchRepository;
import io.github.aloussase.larousse.search.domain.service.SearchService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.assertj.MockMvcTester;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@WebMvcTest(controllers = {SearchController.class})
@Import({SearchService.class, GlobalExceptionHandler.class})
class SearchControllerTest {

    @Autowired
    private MockMvcTester mvc;

    @MockitoBean("larousseSearch")
    private SearchRepository searchRepository;

    @MockitoBean("macacoSearch")
    private SearchRepository macacoSearchRepository;

    @Test
    void testRequestingDefinitionsWithEmptySearchTermReturnsBadRequest() {
        assertThat(
                mvc.get()
                        .uri("/api/v1/search")
                        .param("q", "")
                        .exchange()
        )
                .hasFailed()
                .hasStatus(400);
    }

    @Test
    void testRequestingDefinitionsWithNonEmptySearchTermReturnsListOfDefinitions() {
        final var st = "chat";
        final var def = "animal de maison";
        final var ex = "Le chat est mort";

        when(searchRepository.search(st)).thenReturn(List.of(
                Definition.builder().definition(def).example(Optional.of(ex)).build()
        ));

        assertThat(
                mvc.get()
                        .uri("/api/v1/search")
                        .param("q", st)
                        .exchange()
        )
                .hasStatusOk()
                .bodyJson()
                .hasPathSatisfying("$.[*].definition", x -> x.assertThat().asArray().contains(def))
                .hasPathSatisfying("$.[*].example", x -> x.assertThat().asArray().contains(ex));

        verify(searchRepository).search(st);

    }

}
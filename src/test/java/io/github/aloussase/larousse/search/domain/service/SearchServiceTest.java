package io.github.aloussase.larousse.search.domain.service;

import io.github.aloussase.larousse.core.exception.DomainException;
import io.github.aloussase.larousse.search.domain.entity.Definition;
import io.github.aloussase.larousse.search.domain.repository.SearchRepository;
import org.assertj.core.api.ThrowableAssert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.*;

class SearchServiceTest {

    private SearchRepository searchRepository;

    @BeforeEach
    void setUp() {
        searchRepository = mock(SearchRepository.class);
    }


    @Test
    void testSearchShouldThrowExceptionWhenGivenEmptySearchTerm() {
        // Arrange
        final var service = new SearchService(searchRepository);

        // Act
        final ThrowableAssert.ThrowingCallable invocation = () -> service.search("");

        // Assert
        assertThatThrownBy(invocation)
                .isInstanceOf(DomainException.class)
                .hasMessageContaining("search term");
    }

    @Test
    void testSearchShouldReturnListOfDefinitionsThatMatchSearchTerm() {
        // Arrange
        final var term = "chat";

        when(searchRepository.search(term)).thenReturn(List.of(
                Definition.builder().build(),
                Definition.builder().build()
        ));

        final var service = new SearchService(searchRepository);

        // Act
        final var dfs = service.search(term);

        // Assert
        assertThat(dfs).hasSize(2);

        verify(searchRepository).search(term);
    }

}
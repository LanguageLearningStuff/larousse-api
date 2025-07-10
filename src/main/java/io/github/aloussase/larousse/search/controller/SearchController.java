package io.github.aloussase.larousse.search.controller;

import io.github.aloussase.larousse.search.domain.repository.SearchRepository;
import io.github.aloussase.larousse.search.domain.service.SearchService;
import io.github.aloussase.larousse.search.dto.DefinitionDto;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/search")
public class SearchController {

    private final SearchService searchService;
    private final SearchRepository macacoSearchRepository;
    private final SearchRepository defaultSearchRepository;

    public SearchController(SearchService searchService,
                            @Qualifier("macacoSearch")
                            SearchRepository macacoSearchRepository,
                            @Qualifier("larousseSearch")
                            SearchRepository defaultSearchRepository) {
        this.searchService = searchService;
        this.macacoSearchRepository = macacoSearchRepository;
        this.defaultSearchRepository = defaultSearchRepository;
    }

    @GetMapping
    public ResponseEntity<?> search(
            @RequestParam("q") String searchTerm,
            @RequestParam(value = "lang", required = false, defaultValue = "fr") String lang) {
        final var ss = switch (lang) {
            case "fr" -> defaultSearchRepository;
            case "pt" -> macacoSearchRepository;
            default -> defaultSearchRepository;
        };
        final var defs = searchService.search(ss, searchTerm);
        final var res = defs.stream().map(x ->
                DefinitionDto.builder()
                        .definition(x.getDefinition())
                        .example(x.getExample())
                        .build()
        );
        return ResponseEntity.ok(res);
    }

}

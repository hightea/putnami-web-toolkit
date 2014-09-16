package fr.putnami.pwt.tutorial.shared.service;

import java.util.List;

import fr.putnami.pwt.tutorial.shared.domain.Issue;

public interface IssueService {

	List<Issue> listIssues();

	List<Issue> searchIssues(String name, String label);

	Issue saveIssue(Issue issue);

}

package fr.putnami.pwt.tutorial.server.service;

import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import org.springframework.stereotype.Service;

import fr.putnami.pwt.tutorial.shared.domain.Issue;
import fr.putnami.pwt.tutorial.shared.domain.Type;
import fr.putnami.pwt.tutorial.shared.service.IssueService;

@Service
public class IssueServiceImpl implements IssueService {

	private static final AtomicLong SEQUENCE = new AtomicLong();

	private Map<Long, Issue> issues = Maps.newHashMap();

	@Override
	public List<Issue> listIssues() {
		return Lists.newArrayList(issues.values());
	}

	@Override
	public List<Issue> searchIssues(final String name, final String label) {
		Iterable<Issue> filteredIterable = Iterables.filter(issues.values(), new Predicate<Issue>() {

			@Override
			public boolean apply(Issue issue) {
				boolean result = true;
				if (name != null) {
					result = issue.getName() != null && issue.getName().toLowerCase().contains(name.toLowerCase());
				}
				if (result && label != null) {
					Iterable<String> filteredLabels = Iterables.filter(issue.getLabels(), new Predicate<String>() {

						@Override
						public boolean apply(String issueLabel) {
							return issueLabel != null && issueLabel.toLowerCase().contains(label.toLowerCase());
						}

					});
					result = result && filteredLabels.iterator().hasNext();
				}
				return result;
			}
		});
		return Lists.newArrayList(filteredIterable);
	}

	@Override
	public Issue saveIssue(Issue issue) {
		if (issue.getId() == null) {
			issue.setId(SEQUENCE.incrementAndGet());
		}
		issues.put(issue.getId(), issue);
		return issue;
	}

	public IssueServiceImpl() {
		for (int i = 0; i < ISSUES_NAMES.length; i++) {
			generateIssue(i);
		}
	}

	private void generateIssue(int i) {
		Issue issue = new Issue();
		issue.setId(SEQUENCE.incrementAndGet());
		issue.setName(ISSUES_NAMES[i]);
		issue.getLabels().add(nextValue(ISSUES_LABEL));
		if (new Random().nextBoolean()) {
			issue.getLabels().add("Duplicate");
		}
		if (new Random().nextBoolean()) {
			issue.getLabels().add("Invalid");
		}
		if (new Random().nextBoolean()) {
			issue.getLabels().add("WontFix");
		}
		issue.setDescription("This is a sample description");
		issue.setType(nextValue(Type.values()));
		issues.put(issue.getId(), issue);
	}

	private <T> T nextValue(T[] array) {
		return array[new Random().nextInt(array.length)];
	}

	private static final String[] ISSUES_NAMES = {
			"UI - Image", "Ui - InputRadioBox", "Ui - Progress Bar", "Ui - Icon customisation",
			"Ui - InputCheckBox", "Eclipse GWT classpath", "Code editor - IE issue"
	};

	private static final String[] ISSUES_LABEL = {
			"Bug", "Enhancement", "Question"
	};
}

package fr.putnami.pwt.tutorial.client.issues;

import java.util.Arrays;
import java.util.List;

import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;

import fr.putnami.pwt.core.editor.client.event.FlushSuccessEvent;
import fr.putnami.pwt.core.editor.client.event.ResetDisplayEvent;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.InjectService;
import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.service.client.annotation.AsyncHandler;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.Modal;
import fr.putnami.pwt.core.widget.client.TableEditor;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;
import fr.putnami.pwt.tutorial.shared.constants.IssueConstants;
import fr.putnami.pwt.tutorial.shared.domain.Issue;
import fr.putnami.pwt.tutorial.shared.service.IssueService;

@Templated
public class IssuesView extends Composite implements View {

	public static class SearchBean {
		public String label;
		public String name;
	}

	interface SearchConstants extends ConstantsWithLookup {
		@DefaultStringValue("Label")
		String label();

		@DefaultStringValue("Issue label...")
		String labelPlaceholder();

		@DefaultStringValue("Name")
		String name();

		@DefaultStringValue("Issue name...")
		String namePlaceholder();
	}

	@InjectService
	IssueService service;

	@UiField
	@Initialize(constantsClass = SearchConstants.class)
	Form<SearchBean> searchEditor;

	@UiField
	@Initialize(constantsClass = IssueConstants.class)
	TableEditor<Issue> issuesTable;

	@UiField(provided = true)
	List<String> labelItems = Arrays.asList("Bug", "Enhancement", "Question", "Duplicate", "Invalid", "WontFix");

	@UiField
	@Initialize(constantsClass = IssueConstants.class)
	Form<Issue> issueEditor;

	@UiField
	Modal modal;

	@PresentHandler
	void present(IssuesPlace place) {
		service.listIssues();
	}

	@AsyncHandler
	void onListIssues(List<Issue> issues) {
		issuesTable.edit(issues);
	}

	@UiHandler("searchEditor")
	void onSearchSubmit(FlushSuccessEvent event) {
		SearchBean search = event.getValue();
		service.searchIssues(search.name, search.label);
	}

	@UiHandler("searchEditor")
	void onSearchReset(ResetDisplayEvent event) {
		service.listIssues();
	}

	@AsyncHandler
	void onSearchIssues(List<Issue> issues) {
		issuesTable.edit(issues);
	}

	@UiHandler("addIssue")
	void onAddIssueButton(ButtonEvent event) {
		issueEditor.edit(new Issue());
		modal.show();
	}

	@UiHandler("viewBoutton")
	void onViewButton(ButtonEvent event) {
		Issue issue = event.getValue();
		issueEditor.edit(issue);
		modal.show();
	}

	@UiHandler("issueEditor")
	void onSaveIssueEditor(FlushSuccessEvent event) {
		modal.hide();
		Issue issue = event.getValue();
		service.saveIssue(issue);
	}

	@AsyncHandler
	void onSaveIssue(Issue issue) {
		searchEditor.edit(new SearchBean());
		service.listIssues();
	}
}
